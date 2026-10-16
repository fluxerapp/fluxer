// SPDX-License-Identifier: AGPL-3.0-or-later

export const MAIN_WINDOW_CONTENT_GRACE_MS = 6000;
export const MAIN_WINDOW_HARD_FALLBACK_MS = 10000;

export const MainWindowRevealReason = Object.freeze({
	CONTENT_PAINTED: 'content-painted',
	CONTENT_GRACE_EXPIRED: 'content-grace-expired',
	HARD_FALLBACK: 'hard-fallback',
} as const);

export type MainWindowRevealReason = (typeof MainWindowRevealReason)[keyof typeof MainWindowRevealReason];

type RevealTimer = ReturnType<typeof setTimeout>;

interface MainWindowRevealGateOptions {
	readonly onReveal: (reason: MainWindowRevealReason) => void;
	readonly contentGraceMs?: number;
	readonly hardFallbackMs?: number;
	readonly setTimer?: (callback: () => void, delayMs: number) => RevealTimer;
	readonly clearTimer?: (timer: RevealTimer) => void;
}

export class MainWindowRevealGate {
	private readonly onReveal: (reason: MainWindowRevealReason) => void;
	private readonly contentGraceMs: number;
	private readonly hardFallbackMs: number;
	private readonly setTimer: (callback: () => void, delayMs: number) => RevealTimer;
	private readonly clearTimer: (timer: RevealTimer) => void;
	private graceTimer: RevealTimer | null = null;
	private hardTimer: RevealTimer | null = null;
	private settled = false;

	public constructor(options: MainWindowRevealGateOptions) {
		this.onReveal = options.onReveal;
		this.contentGraceMs = options.contentGraceMs ?? MAIN_WINDOW_CONTENT_GRACE_MS;
		this.hardFallbackMs = options.hardFallbackMs ?? MAIN_WINDOW_HARD_FALLBACK_MS;
		this.setTimer = options.setTimer ?? setTimeout;
		this.clearTimer = options.clearTimer ?? clearTimeout;
	}

	public get revealed(): boolean {
		return this.settled;
	}

	public start(): void {
		if (this.settled || this.hardTimer != null) return;
		this.hardTimer = this.setTimer(() => {
			this.hardTimer = null;
			this.reveal(MainWindowRevealReason.HARD_FALLBACK);
		}, this.hardFallbackMs);
	}

	public markReadyToShow(): void {
		if (this.settled || this.graceTimer != null) return;
		this.graceTimer = this.setTimer(() => {
			this.graceTimer = null;
			this.reveal(MainWindowRevealReason.CONTENT_GRACE_EXPIRED);
		}, this.contentGraceMs);
	}

	public markContentPainted(): void {
		this.reveal(MainWindowRevealReason.CONTENT_PAINTED);
	}

	public dispose(): void {
		this.settled = true;
		this.cancelTimers();
	}

	private reveal(reason: MainWindowRevealReason): void {
		if (this.settled) return;
		this.settled = true;
		this.cancelTimers();
		this.onReveal(reason);
	}

	private cancelTimers(): void {
		if (this.graceTimer != null) {
			this.clearTimer(this.graceTimer);
			this.graceTimer = null;
		}
		if (this.hardTimer != null) {
			this.clearTimer(this.hardTimer);
			this.hardTimer = null;
		}
	}
}
