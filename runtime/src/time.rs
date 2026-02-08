//! Time utilities for FORMA runtime

use std::time::{SystemTime, UNIX_EPOCH};

/// Return the current time in milliseconds since the Unix epoch.
#[no_mangle]
pub extern "C" fn forma_time_now_ms() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0)
}

/// Sleep for the given number of milliseconds.
#[no_mangle]
pub extern "C" fn forma_sleep_ms(ms: i64) {
    if ms > 0 {
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_time_now_ms_positive() {
        let now = forma_time_now_ms();
        assert!(now > 0, "time_now_ms should return positive value");
    }

    #[test]
    fn test_sleep_zero() {
        // Should not panic or hang
        forma_sleep_ms(0);
        forma_sleep_ms(-1);
    }
}
