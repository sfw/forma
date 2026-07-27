//! Lossless source representation used by formatting and editor tooling.
//!
//! The semantic parser intentionally discards trivia. This layer retains every
//! byte so formatting can never erase comments or rewrite literal spellings.

use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxKind {
    Token,
    Whitespace,
    LineComment,
    BlockComment,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxElement {
    pub kind: SyntaxKind,
    pub offset: usize,
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxNodeKind {
    Root,
    Line,
}

/// A structural view over contiguous lossless elements.  Nodes reference the
/// canonical element array rather than copying text, keeping byte reconstruction
/// exact while allowing formatters and editors to work a line at a time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxNode {
    pub kind: SyntaxNodeKind,
    pub byte_range: Range<usize>,
    pub element_range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LosslessSyntax {
    source: String,
    elements: Vec<SyntaxElement>,
    nodes: Vec<SyntaxNode>,
}

impl LosslessSyntax {
    pub fn parse(source: impl Into<String>) -> Self {
        let source = source.into();
        let mut elements = Vec::new();
        let mut offset = 0;
        while offset < source.len() {
            let rest = &source[offset..];
            let (kind, len) = if rest.starts_with("//") || rest.starts_with('#') {
                (SyntaxKind::LineComment, line_len(rest))
            } else if rest.starts_with("/*") {
                (SyntaxKind::BlockComment, block_comment_len(rest))
            } else if rest.starts_with('\n') {
                (SyntaxKind::Whitespace, 1)
            } else if rest.chars().next().is_some_and(char::is_whitespace) {
                (
                    SyntaxKind::Whitespace,
                    rest.char_indices()
                        .find(|(_, ch)| !ch.is_whitespace() || *ch == '\n')
                        .map(|(index, _)| index)
                        .unwrap_or(rest.len()),
                )
            } else {
                (SyntaxKind::Token, token_len(rest))
            };
            let end = offset + len.max(1);
            elements.push(SyntaxElement {
                kind,
                offset,
                text: source[offset..end].to_string(),
            });
            offset = end;
        }
        let mut nodes = vec![SyntaxNode {
            kind: SyntaxNodeKind::Root,
            byte_range: 0..source.len(),
            element_range: 0..elements.len(),
        }];
        let mut line_element_start = 0;
        let mut line_byte_start = 0;
        for (index, element) in elements.iter().enumerate() {
            if element.text.contains('\n') {
                let byte_end = element.offset + element.text.len();
                nodes.push(SyntaxNode {
                    kind: SyntaxNodeKind::Line,
                    byte_range: line_byte_start..byte_end,
                    element_range: line_element_start..index + 1,
                });
                line_element_start = index + 1;
                line_byte_start = byte_end;
            }
        }
        if line_element_start < elements.len() {
            nodes.push(SyntaxNode {
                kind: SyntaxNodeKind::Line,
                byte_range: line_byte_start..source.len(),
                element_range: line_element_start..elements.len(),
            });
        }
        Self {
            source,
            elements,
            nodes,
        }
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn elements(&self) -> &[SyntaxElement] {
        &self.elements
    }

    pub fn root(&self) -> &SyntaxNode {
        &self.nodes[0]
    }

    pub fn lines(&self) -> impl Iterator<Item = &SyntaxNode> {
        self.nodes
            .iter()
            .filter(|node| node.kind == SyntaxNodeKind::Line)
    }

    pub fn elements_for(&self, node: &SyntaxNode) -> &[SyntaxElement] {
        &self.elements[node.element_range.clone()]
    }

    pub fn reconstructed(&self) -> String {
        self.elements
            .iter()
            .map(|element| element.text.as_str())
            .collect()
    }
}

/// Conservative formatter for the lossless tree.
///
/// Until layout rules are represented structurally, it preserves all existing
/// spelling and trivia and only establishes the repository invariant that text
/// files end in exactly one newline. This makes `fmt --write` semantics-safe.
pub struct LosslessFormatter;

impl LosslessFormatter {
    pub fn format(syntax: &LosslessSyntax) -> String {
        let mut output = syntax.source().trim_end_matches('\n').to_string();
        output.push('\n');
        output
    }
}

fn line_len(rest: &str) -> usize {
    rest.find('\n').unwrap_or(rest.len())
}

fn block_comment_len(rest: &str) -> usize {
    rest.find("*/").map(|index| index + 2).unwrap_or(rest.len())
}

fn token_len(rest: &str) -> usize {
    let Some(first) = rest.chars().next() else {
        return 0;
    };
    if first == '"' || first == '\'' || first == '`' {
        return quoted_len(rest, first);
    }
    if first.is_alphanumeric() || first == '_' {
        return rest
            .char_indices()
            .find(|(_, ch)| !ch.is_alphanumeric() && *ch != '_')
            .map(|(index, _)| index)
            .unwrap_or(rest.len());
    }
    first.len_utf8()
}

fn quoted_len(rest: &str, quote: char) -> usize {
    let mut escaped = false;
    for (index, ch) in rest.char_indices().skip(1) {
        if escaped {
            escaped = false;
        } else if ch == '\\' && quote != '`' {
            escaped = true;
        } else if ch == quote {
            return index + ch.len_utf8();
        }
    }
    rest.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reconstruction_is_byte_exact() {
        let source = "# heading\nf main() = r`a  b` // tail\n/* block */\n";
        let syntax = LosslessSyntax::parse(source);
        assert_eq!(syntax.reconstructed(), source);
    }

    #[test]
    fn formatting_preserves_comments_and_literal_spelling() {
        let source = "# keep me\nf main() -> Str = r`a  b` // also keep\n";
        let syntax = LosslessSyntax::parse(source);
        assert_eq!(LosslessFormatter::format(&syntax), source);
    }

    #[test]
    fn structural_lines_cover_the_root_without_losing_bytes() {
        let source = "# first\nf main() = 1 // second\nlast";
        let syntax = LosslessSyntax::parse(source);
        assert_eq!(syntax.root().byte_range, 0..source.len());
        let rebuilt: String = syntax
            .lines()
            .flat_map(|line| syntax.elements_for(line))
            .map(|element| element.text.as_str())
            .collect();
        assert_eq!(rebuilt, source);
    }
}
