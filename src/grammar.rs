//! Authoritative structured grammar model and generated artifacts.

use serde::Serialize;

use crate::lexer::KEYWORDS;

pub const EBNF: &str = include_str!("grammar.ebnf");

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Production {
    pub name: String,
    pub expression: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct KeywordGrammar {
    pub name: String,
    pub canonical: &'static str,
    pub aliases: &'static [&'static str],
    pub contextual: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GrammarModel {
    pub name: &'static str,
    pub version: &'static str,
    pub file_extensions: &'static [&'static str],
    pub productions: Vec<Production>,
    pub keywords: Vec<KeywordGrammar>,
    /// Compatibility view for existing editor consumers of the 0.1 JSON
    /// schema. New consumers should use the top-level structured fields.
    pub rules: GrammarRules,
    pub operators: OperatorGrammar,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct GrammarRules {
    pub keywords: Vec<KeywordGrammar>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct OperatorGrammar {
    pub arithmetic: &'static [&'static str],
    pub comparison: &'static [&'static str],
    pub logical: &'static [&'static str],
    pub bitwise: &'static [&'static str],
    pub assignment: &'static [&'static str],
    pub other: &'static [&'static str],
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EditorGrammar {
    pub scope_name: &'static str,
    pub file_extensions: &'static [&'static str],
    pub keywords: Vec<&'static str>,
    pub operators: Vec<&'static str>,
    pub line_comment: &'static str,
}

pub fn model() -> GrammarModel {
    let keywords: Vec<_> = KEYWORDS
        .iter()
        .map(|keyword| KeywordGrammar {
            name: format!("{:?}", keyword.keyword),
            canonical: keyword.canonical,
            aliases: keyword.aliases,
            contextual: keyword.contextual,
        })
        .collect();
    GrammarModel {
        name: "FORMA",
        version: env!("CARGO_PKG_VERSION"),
        file_extensions: &[".forma"],
        productions: parse_productions(EBNF),
        keywords: keywords.clone(),
        rules: GrammarRules { keywords },
        operators: OperatorGrammar {
            arithmetic: &["+", "-", "*", "/", "%"],
            comparison: &["==", "!=", "<", "<=", ">", ">="],
            logical: &["&&", "||", "!"],
            bitwise: &["&", "|", "^", "<<", ">>"],
            assignment: &["=", ":=", "+=", "-=", "*=", "/=", "%=", "&&=", "||="],
            other: &["->", "=>", "::", ":", ".", "..", "..=", "?", "??", "@"],
        },
    }
}

pub fn editor_metadata() -> EditorGrammar {
    let mut keywords: Vec<_> = KEYWORDS
        .iter()
        .flat_map(|keyword| {
            std::iter::once(keyword.canonical).chain(keyword.aliases.iter().copied())
        })
        .collect();
    keywords.sort_unstable();
    keywords.dedup();
    let grammar = model();
    let mut operators = grammar
        .operators
        .arithmetic
        .iter()
        .chain(grammar.operators.comparison)
        .chain(grammar.operators.logical)
        .chain(grammar.operators.bitwise)
        .chain(grammar.operators.assignment)
        .chain(grammar.operators.other)
        .copied()
        .collect::<Vec<_>>();
    operators.sort_unstable();
    operators.dedup();
    EditorGrammar {
        scope_name: "source.forma",
        file_extensions: &[".forma"],
        keywords,
        operators,
        line_comment: "#",
    }
}

pub fn keyword_markdown() -> String {
    let mut output = String::from(
        "# Forma keyword catalog\n\nGenerated from `src/grammar.rs` and the lexer catalog.\n\n| Canonical | Aliases | Contextual |\n|---|---|---|\n",
    );
    for keyword in model().keywords {
        output.push_str(&format!(
            "| `{}` | {} | {} |\n",
            keyword.canonical,
            if keyword.aliases.is_empty() {
                "—".to_string()
            } else {
                keyword
                    .aliases
                    .iter()
                    .map(|alias| format!("`{alias}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            },
            if keyword.contextual { "yes" } else { "no" }
        ));
    }
    output
}

fn parse_productions(ebnf: &str) -> Vec<Production> {
    let mut uncommented = String::with_capacity(ebnf.len());
    let mut rest = ebnf;
    while let Some(start) = rest.find("(*") {
        uncommented.push_str(&rest[..start]);
        let Some(end) = rest[start + 2..].find("*)") else {
            rest = "";
            break;
        };
        rest = &rest[start + 2 + end + 2..];
    }
    uncommented.push_str(rest);

    uncommented
        .split(';')
        .filter_map(|statement| {
            let (name, expression) = statement.split_once('=')?;
            let name = name.split_whitespace().last()?.trim();
            if name.is_empty() || !name.chars().next().is_some_and(char::is_alphabetic) {
                return None;
            }
            Some(Production {
                name: name.to_string(),
                expression: expression.split_whitespace().collect::<Vec<_>>().join(" "),
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn authoritative_grammar_is_structured_and_unique() {
        let grammar = model();
        assert!(grammar.productions.len() >= 120);
        let mut names = std::collections::HashSet::new();
        for production in &grammar.productions {
            assert!(names.insert(production.name.clone()), "{}", production.name);
            assert!(!production.expression.is_empty(), "{}", production.name);
        }
        for required in ["Program", "Item", "Expression", "Type", "Pattern"] {
            assert!(names.contains(required), "missing {required}");
        }
    }

    #[test]
    fn keyword_aliases_come_from_the_lexer_catalog() {
        let grammar = model();
        assert_eq!(grammar.keywords.len(), KEYWORDS.len());
        assert!(
            grammar
                .keywords
                .iter()
                .any(|keyword| keyword.canonical == "f" && keyword.aliases.contains(&"function"))
        );
    }
}
