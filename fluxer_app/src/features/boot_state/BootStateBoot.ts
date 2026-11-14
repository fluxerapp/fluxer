// SPDX-License-Identifier: AGPL-3.0-or-later

import {bootHydrationPhase, resetBootHydration} from '@app/features/boot_state/BootHydration';
import {afterBootReveal} from '@app/features/boot_state/BootReveal';
import {clearBootState, type PendingBootStateRead, pairBootFrame} from '@app/features/boot_state/BootStateBridge';
import {decodeBootStatePayload} from '@app/features/boot_state/BootStateCodec';
import {discardBootStateSnapshot, hydrateBootState} from '@app/features/boot_state/BootStateHydration';
import BootStateRestore from '@app/features/boot_state/BootStateRestore';
import {rememberRestoredFrameLayout, warmRestoredImages} from '@app/features/boot_state/BootViewport';
import SnapshotSync from '@app/features/gateway/snapshot/SnapshotSync';
import Drafts from '@app/features/messaging/state/MessagingDrafts';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {loadMarkdownParserWasm} from '@app/features/messaging/utils/markdown/parser/MarkdownParserWasm';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('BootStateBoot');

const READ_BUDGET_MS = 400;
const TIMED_OUT: unique symbol = Symbol('timed-out');

function withinBudget<T>(promise: Promise<T>, remainingMs: number): Promise<T | typeof TIMED_OUT> {
	let timer: ReturnType<typeof setTimeout> | undefined;
	const timeout = new Promise<typeof TIMED_OUT>((resolve) => {
		timer = setTimeout(() => resolve(TIMED_OUT), Math.max(0, remainingMs));
	});
	return Promise.race([promise, timeout]).finally(() => clearTimeout(timer));
}

export async function restoreBootState(read: PendingBootStateRead, ready: Promise<unknown>): Promise<boolean> {
	if (!SnapshotSync.enabled) {
		resetBootHydration();
		return false;
	}
	const generation = BootStateRestore.begin();
	if (generation === null) {
		return false;
	}
	const {accountKey, userId} = read;
	const markdownParser = loadMarkdownParserWasm();
	let hydrationStarted = false;
	try {
		const result = await withinBudget(read.result, READ_BUDGET_MS - (performance.now() - read.startedAt));
		if (result === TIMED_OUT || result.status !== 'hit') {
			resetBootHydration();
			BootStateRestore.goCold(generation);
			return false;
		}
		const payload = decodeBootStatePayload(result.payload, {accountKey, userId});
		warmRestoredImages(payload.viewport.imageSources);
		pairBootFrame(accountKey, payload.meta.frame);
		rememberRestoredFrameLayout(payload.meta.frame === null ? null : payload.meta.frame.layout);
		await ready;
		if (!BootStateRestore.isCurrent(generation)) return false;
		hydrationStarted = true;
		const hydrated = hydrateBootState(accountKey, userId, payload);
		await Promise.all([hydrated, markdownParser, Drafts.ready]);
		if (!BootStateRestore.isCurrent(generation)) return false;
		if (bootHydrationPhase() === 'idle' || !BootStateRestore.enterStale(generation)) {
			throw new Error('The boot view did not enter the stale state');
		}
		const channelId = /^\/channels\/[^/]+\/([^/]+)/.exec(location.pathname)?.[1];
		if (channelId !== undefined) {
			afterBootReveal(() => {
				if (SessionManager.currentAccountKey !== accountKey || SessionManager.userId !== userId) return;
				if (/^\/channels\/[^/]+\/([^/]+)/.exec(location.pathname)?.[1] !== channelId) return;
				void Messages.refreshBootChannel(channelId).catch((error: unknown) => {
					logger.warn('Failed to refresh the restored channel', error);
				});
			});
		}
		return true;
	} catch (error) {
		if (!BootStateRestore.isCurrent(generation)) return false;
		logger.warn('Discarding the boot state and booting cold', {accountKey, error});
		if (hydrationStarted) {
			discardBootStateSnapshot(accountKey);
		}
		void clearBootState(accountKey);
		resetBootHydration();
		BootStateRestore.goCold(generation);
		return false;
	}
}
