// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	type ChannelMessagesLoadInput,
	type ChannelMessagesWindowInput,
	type ChannelMessagesWindowStatus,
	createChannelMessagesLoadSnapshot,
	createChannelMessagesWindowSnapshot,
	resolveChannelMessagesLoadDecision,
	resolveChannelMessagesWindowStatus,
	selectChannelMessagesFillerVisible,
	selectChannelMessagesLoadDecision,
	selectChannelMessagesSpacerHeight,
	selectChannelMessagesWindowBar,
	selectChannelMessagesWindowStatus,
	transitionChannelMessagesLoadSnapshot,
	transitionChannelMessagesWindowSnapshot,
} from './ChannelMessagesLoadStateMachine';

function input(overrides: Partial<ChannelMessagesLoadInput> = {}): ChannelMessagesLoadInput {
	return {
		isBefore: false,
		isAfter: false,
		hasJump: false,
		wasReady: true,
		...overrides,
	};
}

describe('channelMessagesLoadMachine', () => {
	it('replaces the visible window for initial, replacement, and jump loads', () => {
		expect(resolveChannelMessagesLoadDecision(input({wasReady: false, isBefore: true}))).toMatchObject({
			mode: 'replace',
			trimTop: false,
			trimBottom: false,
			preserveHasMoreBefore: false,
			preserveHasMoreAfter: false,
		});
		expect(resolveChannelMessagesLoadDecision(input())).toMatchObject({mode: 'replace'});
		expect(resolveChannelMessagesLoadDecision(input({hasJump: true, isAfter: true}))).toMatchObject({
			mode: 'replace',
		});
	});

	it('merges before-pages by prepending and trimming the bottom side', () => {
		expect(resolveChannelMessagesLoadDecision(input({isBefore: true}))).toEqual({
			mode: 'mergeBefore',
			prepend: true,
			trimTop: false,
			trimBottom: true,
			preserveHasMoreBefore: false,
			preserveHasMoreAfter: true,
		});
	});

	it('merges after-pages by appending and trimming the top side', () => {
		expect(resolveChannelMessagesLoadDecision(input({isAfter: true}))).toEqual({
			mode: 'mergeAfter',
			prepend: false,
			trimTop: true,
			trimBottom: false,
			preserveHasMoreBefore: true,
			preserveHasMoreAfter: false,
		});
	});

	it('gives before-pages precedence when both directional flags are present', () => {
		expect(resolveChannelMessagesLoadDecision(input({isBefore: true, isAfter: true}))).toMatchObject({
			mode: 'mergeBefore',
		});
	});

	it('re-routes when load inputs change', () => {
		const beforeSnapshot = createChannelMessagesLoadSnapshot(input({isBefore: true}));
		expect(selectChannelMessagesLoadDecision(beforeSnapshot)).toMatchObject({mode: 'mergeBefore'});

		const replacementSnapshot = transitionChannelMessagesLoadSnapshot(beforeSnapshot, {
			type: 'channelMessagesLoad.updated',
			input: input({hasJump: true}),
		});

		expect(selectChannelMessagesLoadDecision(replacementSnapshot)).toMatchObject({mode: 'replace'});
	});
});

function windowInput(overrides: Partial<ChannelMessagesWindowInput> = {}): ChannelMessagesWindowInput {
	return {
		ready: false,
		loading: false,
		failed: false,
		messageCount: 0,
		hasMoreBefore: true,
		hasMoreAfter: false,
		...overrides,
	};
}

function drawsAFiller(status: ChannelMessagesWindowStatus): boolean {
	return status.olderPageAvailable || status.newerPageAvailable;
}

describe('channelMessagesWindowMachine', () => {
	it('holds a fresh channel in the placeholder phase and asks for a page', () => {
		expect(resolveChannelMessagesWindowStatus(windowInput())).toEqual({
			phase: 'placeholder',
			olderPageAvailable: true,
			newerPageAvailable: false,
			needsPage: true,
			retryVisible: false,
		});
		expect(resolveChannelMessagesWindowStatus(windowInput({loading: true}))).toEqual({
			phase: 'placeholder',
			olderPageAvailable: true,
			newerPageAvailable: false,
			needsPage: false,
			retryVisible: false,
		});
	});

	it('streams a loaded page with its own paging edges', () => {
		expect(
			resolveChannelMessagesWindowStatus(
				windowInput({ready: true, messageCount: 50, hasMoreBefore: true, hasMoreAfter: true}),
			),
		).toEqual({
			phase: 'stream',
			olderPageAvailable: true,
			newerPageAvailable: true,
			needsPage: false,
			retryVisible: false,
		});
		expect(selectChannelMessagesWindowBar(resolveChannelMessagesWindowStatus(windowInput({ready: true})))).toBe('none');
	});

	it('streams a fully loaded empty channel so the welcome header keeps rendering', () => {
		const status = resolveChannelMessagesWindowStatus(
			windowInput({ready: true, hasMoreBefore: false, hasMoreAfter: false}),
		);
		expect(status).toEqual({
			phase: 'stream',
			olderPageAvailable: false,
			newerPageAvailable: false,
			needsPage: false,
			retryVisible: false,
		});
		expect(drawsAFiller(status)).toBe(false);
		expect(selectChannelMessagesWindowBar(status)).toBe('none');
	});

	it('prefers the retry phase over a placeholder when the page failed with nothing loaded', () => {
		const status = resolveChannelMessagesWindowStatus(windowInput({failed: true, hasMoreAfter: true}));
		expect(status).toEqual({
			phase: 'retry',
			olderPageAvailable: true,
			newerPageAvailable: true,
			needsPage: false,
			retryVisible: true,
		});
		expect(drawsAFiller(status)).toBe(true);
		expect(selectChannelMessagesWindowBar(status)).toBe('retry');
	});

	it('keeps a loaded page visible when a follow-up page fails and swaps the bar for retry', () => {
		const status = resolveChannelMessagesWindowStatus(
			windowInput({ready: true, failed: true, messageCount: 50, hasMoreAfter: true}),
		);
		expect(status).toEqual({
			phase: 'stream',
			olderPageAvailable: true,
			newerPageAvailable: true,
			needsPage: false,
			retryVisible: true,
		});
		expect(selectChannelMessagesWindowBar(status)).toBe('retry');
	});

	it('decides the filler from the paging edges alone, never from the phase', () => {
		for (const ready of [false, true]) {
			for (const loading of [false, true]) {
				for (const failed of [false, true]) {
					for (const messageCount of [0, 50]) {
						for (const hasMoreBefore of [false, true]) {
							for (const hasMoreAfter of [false, true]) {
								const status = resolveChannelMessagesWindowStatus({
									ready,
									loading,
									failed,
									messageCount,
									hasMoreBefore,
									hasMoreAfter,
								});
								expect(drawsAFiller(status)).toBe(hasMoreBefore || hasMoreAfter);
								expect(status.phase === 'placeholder' && selectChannelMessagesWindowBar(status) !== 'none').toBe(false);
							}
						}
					}
				}
			}
		}
	});

	it('collapses the empty around-page reply into a placeholder that asks for a page', () => {
		let snapshot = createChannelMessagesWindowSnapshot(windowInput());
		expect(selectChannelMessagesWindowStatus(snapshot)).toEqual({
			phase: 'placeholder',
			olderPageAvailable: true,
			newerPageAvailable: false,
			needsPage: true,
			retryVisible: false,
		});

		snapshot = transitionChannelMessagesWindowSnapshot(snapshot, {
			type: 'channelMessagesWindow.updated',
			input: windowInput({loading: true}),
		});
		expect(selectChannelMessagesWindowStatus(snapshot)).toEqual({
			phase: 'placeholder',
			olderPageAvailable: true,
			newerPageAvailable: false,
			needsPage: false,
			retryVisible: false,
		});

		snapshot = transitionChannelMessagesWindowSnapshot(snapshot, {
			type: 'channelMessagesWindow.updated',
			input: windowInput({ready: true, messageCount: 0, hasMoreBefore: true, hasMoreAfter: true}),
		});
		const afterEmptyAroundPage = selectChannelMessagesWindowStatus(snapshot);
		expect(afterEmptyAroundPage).toEqual({
			phase: 'placeholder',
			olderPageAvailable: true,
			newerPageAvailable: true,
			needsPage: true,
			retryVisible: false,
		});
		expect(drawsAFiller(afterEmptyAroundPage)).toBe(true);
		expect(selectChannelMessagesWindowBar(afterEmptyAroundPage)).toBe('none');

		snapshot = transitionChannelMessagesWindowSnapshot(snapshot, {
			type: 'channelMessagesWindow.updated',
			input: windowInput({ready: true, messageCount: 0, hasMoreBefore: false, hasMoreAfter: false}),
		});
		expect(selectChannelMessagesWindowStatus(snapshot)).toEqual({
			phase: 'stream',
			olderPageAvailable: false,
			newerPageAvailable: false,
			needsPage: false,
			retryVisible: false,
		});
	});
});

const FILLER_HEIGHT = 240;

function loadedWindowInput(overrides: Partial<ChannelMessagesWindowInput> = {}): ChannelMessagesWindowInput {
	return {
		ready: true,
		loading: false,
		failed: false,
		messageCount: 40,
		hasMoreBefore: false,
		hasMoreAfter: false,
		...overrides,
	};
}

function spacerFor(overrides: Partial<ChannelMessagesWindowInput> = {}): number {
	return selectChannelMessagesSpacerHeight(
		resolveChannelMessagesWindowStatus(loadedWindowInput(overrides)),
		FILLER_HEIGHT,
	);
}

describe('selectChannelMessagesSpacerHeight', () => {
	it('reports the filler height during retry too, because retry now draws the same list', () => {
		const status = resolveChannelMessagesWindowStatus(
			loadedWindowInput({ready: false, failed: true, messageCount: 0, hasMoreBefore: true, hasMoreAfter: true}),
		);
		expect(status.phase).toBe('retry');
		expect(selectChannelMessagesSpacerHeight(status, FILLER_HEIGHT)).toBe(FILLER_HEIGHT);
	});

	it('withholds the filler height during retry once both paging edges are closed', () => {
		const status = resolveChannelMessagesWindowStatus(
			loadedWindowInput({ready: false, failed: true, messageCount: 0, hasMoreBefore: false, hasMoreAfter: false}),
		);
		expect(status.phase).toBe('retry');
		expect(selectChannelMessagesSpacerHeight(status, FILLER_HEIGHT)).toBe(0);
	});

	it('withholds the filler height from a stream with both paging edges closed', () => {
		expect(spacerFor({hasMoreBefore: false, hasMoreAfter: false})).toBe(0);
	});

	it('reports the filler height for whichever single stream edge is still open', () => {
		expect(spacerFor({hasMoreBefore: true, hasMoreAfter: false})).toBe(FILLER_HEIGHT);
		expect(spacerFor({hasMoreBefore: false, hasMoreAfter: true})).toBe(FILLER_HEIGHT);
		expect(spacerFor({hasMoreBefore: true, hasMoreAfter: true})).toBe(FILLER_HEIGHT);
	});

	it('reports the filler height for a placeholder whose older edge is open', () => {
		expect(spacerFor({ready: false, messageCount: 0, hasMoreBefore: true})).toBe(FILLER_HEIGHT);
		expect(spacerFor({ready: false, loading: true, messageCount: 0, hasMoreBefore: true})).toBe(FILLER_HEIGHT);
	});

	it('withholds it from a placeholder with both paging edges closed, which draws no filler', () => {
		expect(spacerFor({ready: false, messageCount: 0, hasMoreBefore: false, hasMoreAfter: false})).toBe(0);
	});

	it('is the filler height exactly when a paging edge is open, in every phase', () => {
		const seen = new Set<string>();
		let zeroCases = 0;
		let fillerCases = 0;
		for (const ready of [false, true]) {
			for (const loading of [false, true]) {
				for (const failed of [false, true]) {
					for (const messageCount of [0, 40]) {
						for (const hasMoreBefore of [false, true]) {
							for (const hasMoreAfter of [false, true]) {
								const input = {ready, loading, failed, messageCount, hasMoreBefore, hasMoreAfter};
								const status = resolveChannelMessagesWindowStatus(input);
								const height = selectChannelMessagesSpacerHeight(status, FILLER_HEIGHT);
								expect(height).toBe(hasMoreBefore || hasMoreAfter ? FILLER_HEIGHT : 0);
								seen.add(status.phase);
								if (height === 0) zeroCases += 1;
								else fillerCases += 1;
							}
						}
					}
				}
			}
		}
		expect([...seen].sort()).toEqual(['placeholder', 'retry', 'stream']);
		expect(zeroCases).toBeGreaterThan(0);
		expect(fillerCases).toBeGreaterThan(0);
	});
});

describe('selectChannelMessagesFillerVisible', () => {
	it('always draws the filler when motion is not reduced', () => {
		for (const scrollManagerInitialized of [false, true]) {
			for (const ready of [false, true]) {
				expect(selectChannelMessagesFillerVisible({reducedMotion: false, scrollManagerInitialized, ready})).toBe(true);
			}
		}
	});

	it('holds the filler back under reduced motion until the manager is initialized or the page is ready', () => {
		expect(
			selectChannelMessagesFillerVisible({reducedMotion: true, scrollManagerInitialized: false, ready: false}),
		).toBe(false);
		expect(
			selectChannelMessagesFillerVisible({reducedMotion: true, scrollManagerInitialized: true, ready: false}),
		).toBe(true);
		expect(
			selectChannelMessagesFillerVisible({reducedMotion: true, scrollManagerInitialized: false, ready: true}),
		).toBe(true);
		expect(selectChannelMessagesFillerVisible({reducedMotion: true, scrollManagerInitialized: true, ready: true})).toBe(
			true,
		);
	});

	it('cannot suppress the stream filler, because the stream phase is only reachable when the page is ready', () => {
		let streamCases = 0;
		for (const ready of [false, true]) {
			for (const loading of [false, true]) {
				for (const failed of [false, true]) {
					for (const messageCount of [0, 40]) {
						for (const hasMoreBefore of [false, true]) {
							for (const hasMoreAfter of [false, true]) {
								const input = {ready, loading, failed, messageCount, hasMoreBefore, hasMoreAfter};
								if (resolveChannelMessagesWindowStatus(input).phase !== 'stream') continue;
								streamCases += 1;
								expect(input.ready).toBe(true);
								expect(
									selectChannelMessagesFillerVisible({
										reducedMotion: true,
										scrollManagerInitialized: false,
										ready: input.ready,
									}),
								).toBe(true);
							}
						}
					}
				}
			}
		}
		expect(streamCases).toBeGreaterThan(0);
	});
});
