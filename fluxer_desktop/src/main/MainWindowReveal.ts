// SPDX-License-Identifier: AGPL-3.0-or-later

const MAIN_WINDOW_CONTENT_GRACE_MS = 6000;
const MAIN_WINDOW_HARD_FALLBACK_MS = 10000;

export const MainWindowRevealReason = Object.freeze({
	CONTENT_PAINTED: 'content-painted',
	CONTENT_GRACE_EXPIRED: 'content-grace-expired',
	HARD_FALLBACK: 'hard-fallback',
} as const);

export type MainWindowRevealReason = (typeof MainWindowRevealReason)[keyof typeof MainWindowRevealReason];

type RevealTimer = ReturnType<typeof setTimeout>;

interface MainWindowRevealGateOptions {
	readonly onReveal: (reason: MainWindowRevealReason) => void;
}

export class MainWindowRevealGate {
	private readonly onReveal: (reason: MainWindowRevealReason) => void;
	private graceTimer: RevealTimer | null = null;
	private hardTimer: RevealTimer | null = null;
	private settled = false;

	public constructor(options: MainWindowRevealGateOptions) {
		this.onReveal = options.onReveal;
	}

	public get revealed(): boolean {
		return this.settled;
	}

	public start(): void {
		if (this.settled || this.hardTimer != null) return;
		this.hardTimer = setTimeout(() => {
			this.hardTimer = null;
			this.reveal(MainWindowRevealReason.HARD_FALLBACK);
		}, MAIN_WINDOW_HARD_FALLBACK_MS);
	}

	public markReadyToShow(): void {
		if (this.settled || this.graceTimer != null) return;
		this.graceTimer = setTimeout(() => {
			this.graceTimer = null;
			this.reveal(MainWindowRevealReason.CONTENT_GRACE_EXPIRED);
		}, MAIN_WINDOW_CONTENT_GRACE_MS);
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
			clearTimeout(this.graceTimer);
			this.graceTimer = null;
		}
		if (this.hardTimer != null) {
			clearTimeout(this.hardTimer);
			this.hardTimer = null;
		}
	}
}
