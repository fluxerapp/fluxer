// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU32, Ordering};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u32)]
pub enum State {
    Idle = 0,
    Starting = 1,
    Running = 2,
    Stopping = 3,
    Stopped = 4,
}

impl State {
    fn from_u32(v: u32) -> State {
        match v {
            0 => State::Idle,
            1 => State::Starting,
            2 => State::Running,
            3 => State::Stopping,
            _ => State::Stopped,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TransitionError {
    IllegalTransition,
    DoubleStart,
    StopBeforeStart,
    StartWhileStopping,
}

pub fn is_allowed(from: State, to: State) -> bool {
    match from {
        State::Idle => matches!(to, State::Starting | State::Stopped),
        State::Starting => matches!(to, State::Running | State::Stopped),
        State::Running => matches!(to, State::Stopping | State::Stopped),
        State::Stopping => matches!(to, State::Stopped),
        State::Stopped => false,
    }
}

pub struct Machine {
    state: AtomicU32,
}

impl Machine {
    pub fn new() -> Self {
        Self {
            state: AtomicU32::new(State::Idle as u32),
        }
    }

    pub fn current(&self) -> State {
        State::from_u32(self.state.load(Ordering::Acquire))
    }

    fn cas(&self, from: State, to: State) -> Result<(), TransitionError> {
        if !is_allowed(from, to) {
            return Err(TransitionError::IllegalTransition);
        }
        self.state
            .compare_exchange(from as u32, to as u32, Ordering::AcqRel, Ordering::Acquire)
            .map(|_| ())
            .map_err(|_| TransitionError::IllegalTransition)
    }

    pub fn request_start(&self) -> Result<(), TransitionError> {
        match self.current() {
            State::Idle => self.cas(State::Idle, State::Starting),
            State::Starting | State::Running => Err(TransitionError::DoubleStart),
            State::Stopping => Err(TransitionError::StartWhileStopping),
            State::Stopped => Err(TransitionError::IllegalTransition),
        }
    }

    pub fn mark_running(&self) -> Result<(), TransitionError> {
        self.cas(State::Starting, State::Running)
    }

    pub fn request_stop(&self) -> Result<(), TransitionError> {
        match self.current() {
            State::Running => self.cas(State::Running, State::Stopping),
            State::Idle => Err(TransitionError::StopBeforeStart),
            State::Starting | State::Stopping | State::Stopped => {
                Err(TransitionError::IllegalTransition)
            }
        }
    }

    pub fn mark_stopped(&self) -> Result<(), TransitionError> {
        self.cas(State::Stopping, State::Stopped)
    }

    pub fn cancel_idle(&self) -> Result<(), TransitionError> {
        self.cas(State::Idle, State::Stopped)
    }

    pub fn mark_fatal(&self) -> State {
        loop {
            let raw = self.state.load(Ordering::Acquire);
            let prev = State::from_u32(raw);
            if prev == State::Stopped {
                return prev;
            }
            if self
                .state
                .compare_exchange(
                    raw,
                    State::Stopped as u32,
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
            {
                return prev;
            }
        }
    }
}

impl Default for Machine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn happy_path() {
        let m = Machine::new();
        assert_eq!(State::Idle, m.current());
        m.request_start().unwrap();
        assert_eq!(State::Starting, m.current());
        m.mark_running().unwrap();
        assert_eq!(State::Running, m.current());
        m.request_stop().unwrap();
        assert_eq!(State::Stopping, m.current());
        m.mark_stopped().unwrap();
        assert_eq!(State::Stopped, m.current());
    }

    #[test]
    fn double_start_rejected() {
        let m = Machine::new();
        m.request_start().unwrap();
        assert_eq!(Err(TransitionError::DoubleStart), m.request_start());
        m.mark_running().unwrap();
        assert_eq!(Err(TransitionError::DoubleStart), m.request_start());
    }

    #[test]
    fn stop_before_start_rejected() {
        let m = Machine::new();
        assert_eq!(Err(TransitionError::StopBeforeStart), m.request_stop());
    }

    #[test]
    fn start_while_stopping_rejected() {
        let m = Machine::new();
        m.request_start().unwrap();
        m.mark_running().unwrap();
        m.request_stop().unwrap();
        assert_eq!(Err(TransitionError::StartWhileStopping), m.request_start());
    }

    #[test]
    fn cancel_idle_short_circuits() {
        let m = Machine::new();
        m.cancel_idle().unwrap();
        assert_eq!(State::Stopped, m.current());
        assert_eq!(Err(TransitionError::IllegalTransition), m.request_start());
    }

    #[test]
    fn mark_fatal_forces_stopped() {
        for &start in &[
            State::Idle,
            State::Starting,
            State::Running,
            State::Stopping,
        ] {
            let m = Machine::new();
            m.state.store(start as u32, Ordering::Release);
            let prev = m.mark_fatal();
            assert_eq!(start, prev);
            assert_eq!(State::Stopped, m.current());
        }
    }

    #[test]
    fn mark_fatal_idempotent() {
        let m = Machine::new();
        m.state.store(State::Stopped as u32, Ordering::Release);
        let prev = m.mark_fatal();
        assert_eq!(State::Stopped, prev);
        assert_eq!(State::Stopped, m.current());
    }
}
