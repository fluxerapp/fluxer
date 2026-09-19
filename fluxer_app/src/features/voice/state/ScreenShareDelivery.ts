// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	clearScreenShareDeliveryMemoryKey,
	readScreenShareDeliveryMemory,
	type ScreenShareDeliveryDescription,
	type ScreenShareDeliveryEvaluation,
	type ScreenShareDeliveryMemory,
	type ScreenShareDeliveryMemoryEntry,
	type ScreenShareDeliveryMemoryHeader,
	type ScreenShareDeliveryMemoryKey,
	type ScreenShareDeliveryMemoryWrite,
	type ScreenShareDeliveryNotice,
	type ScreenShareDeliveryShareCounters,
	shouldToastScreenShareNotice,
	updateScreenShareDeliveryMemory,
} from '@app/features/voice/engine/ScreenShareUnderperformance';
import type {
	ScreenShareContext,
	ScreenShareLevel,
	ScreenShareTarget,
} from '@app/features/voice/utils/ScreenShareOptions';
import type {VideoCodec} from 'livekit-client';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('ScreenShareDelivery');
const MEMORY_STORAGE_KEY = 'ScreenShareDeliveryMemoryV1';

export interface ScreenShareDeliveryLadder {
	target: ScreenShareTarget;
	context: ScreenShareContext;
	levels: ReadonlyArray<ScreenShareLevel>;
}

export interface ScreenShareDeliveryShareInput extends ScreenShareDeliveryLadder {
	header: ScreenShareDeliveryMemoryHeader;
	hardware: Partial<Record<VideoCodec, boolean>>;
	adaptive: boolean;
}

export interface ScreenShareDeliverySnapshot {
	share: ScreenShareDeliveryShareInput | null;
	notice: ScreenShareDeliveryNotice | null;
	plan: ScreenShareDeliveryPlan | null;
	counters: ScreenShareDeliveryShareCounters;
	verdicts: Partial<Record<VideoCodec, ScreenShareDeliveryMemoryEntry>>;
	multiLayer: boolean;
}

export interface ScreenShareDeliveryPlan {
	target: ScreenShareTarget;
	codec: VideoCodec;
	level: ScreenShareLevel;
	levelIndex: number;
	deliveredFrameRate: number | null;
	sentWidth: number | null;
	sentHeight: number | null;
	sourceFrameRate: number | null;
	holdingShort: boolean;
}

function createShareCounters(): ScreenShareDeliveryShareCounters {
	return {switches: 0, probes: 0, layerings: 0, probeBlocked: false, toastShown: false};
}

function isStoredMemory(value: unknown): value is ScreenShareDeliveryMemory {
	if (typeof value !== 'object' || value === null) return false;
	const memory = value as ScreenShareDeliveryMemory;
	return (
		typeof memory.header === 'object' &&
		memory.header !== null &&
		typeof memory.entries === 'object' &&
		memory.entries !== null
	);
}

function readStoredMemory(): ScreenShareDeliveryMemory | null {
	try {
		const raw = AppStorage.getItem(MEMORY_STORAGE_KEY);
		if (raw === null) return null;
		const parsed: unknown = JSON.parse(raw);
		return isStoredMemory(parsed) ? parsed : null;
	} catch (error) {
		logger.warn('Failed to read the screen share delivery memory', error);
		return null;
	}
}

function writeStoredMemory(memory: ScreenShareDeliveryMemory): void {
	try {
		AppStorage.setItem(MEMORY_STORAGE_KEY, JSON.stringify(memory));
	} catch (error) {
		logger.warn('Failed to store the screen share delivery memory', error);
	}
}

class ScreenShareDelivery {
	notice: ScreenShareDeliveryNotice | null = null;
	plan: ScreenShareDeliveryPlan | null = null;
	counters: ScreenShareDeliveryShareCounters = createShareCounters();
	verdicts: Partial<Record<VideoCodec, ScreenShareDeliveryMemoryEntry>> = {};
	multiLayer = false;
	private share: ScreenShareDeliveryShareInput | null = null;

	constructor() {
		makeAutoObservable(this, undefined, {autoBind: true});
	}

	get isSharing(): boolean {
		return this.share !== null;
	}

	get ladder(): ScreenShareDeliveryLadder | null {
		return this.share;
	}

	get adaptive(): boolean {
		return this.share?.adaptive === true;
	}

	snapshot(): ScreenShareDeliverySnapshot {
		return {
			share: this.share,
			notice: this.notice,
			plan: this.plan,
			counters: {...this.counters},
			verdicts: this.verdicts,
			multiLayer: this.multiLayer,
		};
	}

	restore(snapshot: ScreenShareDeliverySnapshot): void {
		this.share = snapshot.share;
		this.notice = snapshot.notice;
		this.plan = snapshot.plan;
		this.counters = {...snapshot.counters};
		this.verdicts = snapshot.verdicts;
		this.multiLayer = snapshot.multiLayer;
	}

	beginShare(share: ScreenShareDeliveryShareInput): void {
		this.share = share;
		this.notice = null;
		this.plan = null;
		this.counters = createShareCounters();
		this.verdicts = this.readVerdicts();
		this.multiLayer = false;
	}

	retarget(ladder: ScreenShareDeliveryLadder): void {
		if (this.share === null) return;
		this.share = {...this.share, ...ladder};
		this.notice = null;
		this.plan = null;
		this.verdicts = this.readVerdicts();
	}

	demoteForShare(codec: VideoCodec, ratio: number): void {
		if (this.share === null) return;
		this.verdicts = {
			...this.verdicts,
			[codec]: {short: true, ratio, settledIndex: null, probeBlockedUntil: null, at: Date.now()},
		};
	}

	record(
		codec: VideoCodec,
		evaluation: ScreenShareDeliveryEvaluation,
		description: ScreenShareDeliveryDescription,
	): boolean {
		if (this.share === null || !this.share.adaptive) return false;
		this.plan = {
			target: this.share.target,
			codec,
			level: description.level,
			levelIndex: description.levelIndex,
			deliveredFrameRate: description.deliveredFrameRate,
			sentWidth: description.sentWidth,
			sentHeight: description.sentHeight,
			sourceFrameRate: description.sourceFrameRate,
			holdingShort: description.holdingShort,
		};
		if (evaluation.decision.kind === 'switch-codec') {
			this.counters.switches += 1;
		} else if (evaluation.decision.kind === 'republish-layering') {
			this.counters.layerings += 1;
		} else if (evaluation.decision.kind === 'probe') {
			this.counters.probes += 1;
		} else if (evaluation.decision.kind === 'revert-probe') {
			this.counters.probeBlocked = true;
		}
		if (evaluation.memoryWrite !== null) {
			this.writeMemory(codec, evaluation.memoryWrite);
		}
		return evaluation.noticeChanged ? this.setNotice(evaluation.notice) : false;
	}

	setMultiLayer(multiLayer: boolean): void {
		this.multiLayer = multiLayer;
	}

	setNotice(notice: ScreenShareDeliveryNotice | null): boolean {
		this.notice = notice;
		if (!shouldToastScreenShareNotice(this.counters, notice)) return false;
		this.counters.toastShown = true;
		return true;
	}

	resetToFullQuality(): void {
		const key = this.buildKey();
		if (key !== null) {
			writeStoredMemory(clearScreenShareDeliveryMemoryKey(readStoredMemory(), key));
		}
		this.notice = null;
		this.counters = createShareCounters();
		this.verdicts = {};
	}

	endShare(): void {
		this.share = null;
		this.notice = null;
		this.plan = null;
		this.counters = createShareCounters();
		this.verdicts = {};
		this.multiLayer = false;
	}

	private buildKey(): ScreenShareDeliveryMemoryKey | null {
		if (this.share === null || !this.share.adaptive) return null;
		return {
			header: this.share.header,
			memoryKey: this.share.target.memoryKey,
			hardware: this.share.hardware,
			now: Date.now(),
		};
	}

	private readVerdicts(): Partial<Record<VideoCodec, ScreenShareDeliveryMemoryEntry>> {
		const key = this.buildKey();
		return key === null ? {} : readScreenShareDeliveryMemory(readStoredMemory(), key);
	}

	private writeMemory(codec: VideoCodec, write: ScreenShareDeliveryMemoryWrite): void {
		const key = this.buildKey();
		if (key === null) return;
		writeStoredMemory(updateScreenShareDeliveryMemory(readStoredMemory(), key, codec, write));
	}
}

export default new ScreenShareDelivery();
