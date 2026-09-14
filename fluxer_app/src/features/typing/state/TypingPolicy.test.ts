// SPDX-License-Identifier: AGPL-3.0-or-later

import {INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import type {TypingIndicatorReworkAssignmentResponse} from '@fluxer/schema/src/domains/experiment/TypingIndicatorReworkSchemas';
import {runInAction} from 'mobx';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const doubles = vi.hoisted(() => ({
	post: vi.fn((_path: string) => Promise.resolve({ok: true})),
}));

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({http: {get: vi.fn(), post: doubles.post}}));
vi.mock('@app/features/auth/state/Authentication', () => ({default: {currentUserId: 'me'}}));
vi.mock('@app/features/devtools/state/DeveloperOptions', () => ({default: {showMyselfTyping: false}}));
vi.mock('@app/features/relationship/state/Relationships', () => ({default: {isBlocked: () => false}}));
vi.mock('@app/features/user/state/Users', () => ({default: {getUser: () => undefined}}));

const {ExperimentAssignments} = await import('@app/features/experiment/state/ExperimentAssignments');
const {default: TypingPolicy} = await import('@app/features/typing/state/TypingPolicy');
const {default: LegacyTypingIndicator} = await import('@app/features/typing/legacy/LegacyTypingIndicator');
const {TypingUtils: LegacyTypingUtils} = await import('@app/features/typing/legacy/LegacyTypingUtils');
const {default: RollingTypingSender} = await import('@app/features/typing/rolling/RollingTypingSender');
const {default: RollingTypingStore} = await import('@app/features/typing/rolling/RollingTypingStore');

const START_TIME = 2_000_000;
const CHANNEL = 'channel';

const TARGETED_ASSIGNMENT: TypingIndicatorReworkAssignmentResponse = {
	enabled: true,
	config_version: 7,
	user_targeted: true,
	source: 'canary',
};
const DISABLED_ASSIGNMENT: TypingIndicatorReworkAssignmentResponse = {
	enabled: false,
	config_version: 8,
	user_targeted: false,
	source: null,
};

function publish(assignment: TypingIndicatorReworkAssignmentResponse | undefined): void {
	runInAction(() => {
		ExperimentAssignments.response = {
			poll_interval_seconds: 300,
			poll_jitter_percent: 15,
			assignments: assignment === undefined ? {} : {typing_indicator_rework: assignment},
		};
	});
}

function advanceTo(elapsedMs: number): void {
	vi.advanceTimersByTime(elapsedMs - (Date.now() - START_TIME));
}

beforeEach(() => {
	vi.useFakeTimers();
	vi.setSystemTime(START_TIME);
});

afterEach(() => {
	TypingPolicy.stop();
	runInAction(() => {
		ExperimentAssignments.response = INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE;
	});
	LegacyTypingIndicator.reset();
	RollingTypingSender.reset();
	RollingTypingStore.reset();
	vi.useRealTimers();
	vi.clearAllMocks();
});

describe('TypingPolicy', () => {
	it('starts on the legacy policy before any assignment has landed', () => {
		TypingPolicy.start();

		expect(TypingPolicy.active).toBe('legacy');
	});

	it('starts on the legacy policy before the policy is started', () => {
		publish(TARGETED_ASSIGNMENT);

		expect(TypingPolicy.active).toBe('legacy');
	});

	it('stays on the legacy policy when the experiment key is missing', () => {
		TypingPolicy.start();

		publish(undefined);

		expect(TypingPolicy.active).toBe('legacy');
	});

	it('moves to the rolling policy when the first poll after ready targets the account', () => {
		TypingPolicy.start();

		publish(TARGETED_ASSIGNMENT);

		expect(TypingPolicy.active).toBe('rolling');
	});

	it('moves back to the legacy policy when a later poll turns the rollout off', () => {
		TypingPolicy.start();
		publish(TARGETED_ASSIGNMENT);

		publish(DISABLED_ASSIGNMENT);

		expect(TypingPolicy.active).toBe('legacy');
	});

	it('never resets the legacy store while the assignment stays untargeted', () => {
		TypingPolicy.start();
		LegacyTypingIndicator.startRemoteTyping(CHANNEL, 'alice');

		publish({...TARGETED_ASSIGNMENT, user_targeted: false, source: null});
		publish(DISABLED_ASSIGNMENT);
		publish(undefined);

		expect(LegacyTypingIndicator.isTyping(CHANNEL, 'alice')).toBe(true);
	});

	it('releases the legacy send and idle timers when flipping to rolling', () => {
		TypingPolicy.start();
		LegacyTypingUtils.typing(CHANNEL);
		expect(vi.getTimerCount()).toBe(3);

		publish(TARGETED_ASSIGNMENT);

		expect(vi.getTimerCount()).toBe(0);
		vi.advanceTimersByTime(30000);
		expect(doubles.post).not.toHaveBeenCalled();
	});

	it('releases the legacy cooldown when flipping to rolling', () => {
		TypingPolicy.start();
		LegacyTypingUtils.typing(CHANNEL);
		advanceTo(1500);
		expect(doubles.post).toHaveBeenCalledTimes(1);

		publish(TARGETED_ASSIGNMENT);
		publish(DISABLED_ASSIGNMENT);
		advanceTo(1600);
		LegacyTypingUtils.typing(CHANNEL);
		advanceTo(3100);

		expect(doubles.post).toHaveBeenCalledTimes(2);
	});

	it('releases the rolling send slot and every entry timer when flipping to legacy', () => {
		TypingPolicy.start();
		publish(TARGETED_ASSIGNMENT);
		RollingTypingSender.startTyping(CHANNEL);
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		expect(vi.getTimerCount()).toBe(3);

		publish(DISABLED_ASSIGNMENT);

		expect(vi.getTimerCount()).toBe(0);
		vi.advanceTimersByTime(30000);
		expect(doubles.post).not.toHaveBeenCalled();
	});

	it('leaves no timers armed straight after a flip with pending work on both arms', () => {
		TypingPolicy.start();
		LegacyTypingUtils.typing(CHANNEL);
		LegacyTypingIndicator.startRemoteTyping(CHANNEL, 'alice');
		RollingTypingSender.startTyping(CHANNEL);
		RollingTypingStore.start(CHANNEL, 'bob', 'gateway');
		expect(vi.getTimerCount()).toBeGreaterThanOrEqual(5);

		publish(TARGETED_ASSIGNMENT);

		expect(vi.getTimerCount()).toBe(0);
	});

	it('never posts twice from a single pending send across a flip', () => {
		TypingPolicy.start();
		publish(TARGETED_ASSIGNMENT);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(500);
		publish(DISABLED_ASSIGNMENT);
		advanceTo(700);
		publish(TARGETED_ASSIGNMENT);
		advanceTo(800);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(6000);
		expect(doubles.post).toHaveBeenCalledTimes(1);

		publish(DISABLED_ASSIGNMENT);
		LegacyTypingUtils.typing(CHANNEL);
		advanceTo(6500);
		publish(TARGETED_ASSIGNMENT);
		advanceTo(6700);
		publish(DISABLED_ASSIGNMENT);
		LegacyTypingUtils.typing(CHANNEL);
		advanceTo(12000);
		expect(doubles.post).toHaveBeenCalledTimes(2);
	});

	it('starts the rolling sender with a 1500 ms first send after a flip', () => {
		TypingPolicy.start();
		LegacyTypingUtils.typing(CHANNEL);
		advanceTo(1500);
		expect(doubles.post).toHaveBeenCalledTimes(1);

		advanceTo(2000);
		publish(TARGETED_ASSIGNMENT);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(3499);
		expect(doubles.post).toHaveBeenCalledTimes(1);

		advanceTo(3500);
		expect(doubles.post).toHaveBeenCalledTimes(2);
	});

	it('clears remote typists from both stores on every flip', () => {
		TypingPolicy.start();
		LegacyTypingIndicator.startRemoteTyping(CHANNEL, 'alice');
		RollingTypingStore.start(CHANNEL, 'bob', 'gateway');

		publish(TARGETED_ASSIGNMENT);
		expect(LegacyTypingIndicator.isTyping(CHANNEL, 'alice')).toBe(false);
		expect(RollingTypingStore.isTyping(CHANNEL, 'bob')).toBe(false);

		LegacyTypingIndicator.startRemoteTyping(CHANNEL, 'alice');
		RollingTypingStore.start(CHANNEL, 'bob', 'gateway');
		publish(DISABLED_ASSIGNMENT);
		expect(LegacyTypingIndicator.isTyping(CHANNEL, 'alice')).toBe(false);
		expect(RollingTypingStore.isTyping(CHANNEL, 'bob')).toBe(false);
	});

	it('ignores a poll that repeats the current assignment', () => {
		TypingPolicy.start();
		publish(TARGETED_ASSIGNMENT);
		RollingTypingStore.start(CHANNEL, 'bob', 'gateway');
		RollingTypingSender.startTyping(CHANNEL);

		publish({...TARGETED_ASSIGNMENT});
		publish({...TARGETED_ASSIGNMENT, config_version: 9});

		expect(TypingPolicy.active).toBe('rolling');
		expect(RollingTypingStore.isTyping(CHANNEL, 'bob')).toBe(true);
		vi.advanceTimersByTime(1500);
		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('returns to legacy when experiment assignments are reset on logout', () => {
		TypingPolicy.start();
		publish(TARGETED_ASSIGNMENT);
		RollingTypingSender.startTyping(CHANNEL);

		ExperimentAssignments.reset();

		expect(TypingPolicy.active).toBe('legacy');
		expect(vi.getTimerCount()).toBe(0);
	});

	it('returns to legacy and releases every timer when stopped', () => {
		TypingPolicy.start();
		publish(TARGETED_ASSIGNMENT);
		RollingTypingSender.startTyping(CHANNEL);
		RollingTypingStore.start(CHANNEL, 'bob', 'gateway');

		TypingPolicy.stop();

		expect(TypingPolicy.active).toBe('legacy');
		expect(vi.getTimerCount()).toBe(0);
		publish(DISABLED_ASSIGNMENT);
		publish(TARGETED_ASSIGNMENT);
		expect(TypingPolicy.active).toBe('legacy');
	});

	it('starts twice without installing a second reaction', () => {
		TypingPolicy.start();
		TypingPolicy.start();
		TypingPolicy.stop();

		publish(TARGETED_ASSIGNMENT);

		expect(TypingPolicy.active).toBe('legacy');
	});
});
