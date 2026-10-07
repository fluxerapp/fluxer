// SPDX-License-Identifier: AGPL-3.0-or-later

const INITIAL_RETRY_DELAY_MS = 1000;
const MAX_RETRY_DELAY_MS = 30000;
const FAILURES_BEFORE_PROMPT = 3;
const ERR_ABORTED = -3;

export interface AppLoadFailure {
	errorCode: number;
	errorDescription: string;
	url: string;
}

interface AppLoadRetryLogger {
	info(message: string, detail?: Record<string, unknown>): void;
	warn(message: string, detail?: Record<string, unknown>): void;
	error(message: string, detail?: Record<string, unknown>): void;
}

interface AppLoadRetryOptions {
	webContents: Pick<Electron.WebContents, 'on' | 'isDestroyed' | 'isLoadingMainFrame' | 'loadURL'>;
	appUrl: string;
	logger: AppLoadRetryLogger;
	isTrustedUrl: (url: string) => boolean;
	onRepeatedFailure: (failure: AppLoadFailure) => void;
	onCommitted: () => void;
	setTimer?: (callback: () => void, delayMs: number) => ReturnType<typeof setTimeout>;
	clearTimer?: (timer: ReturnType<typeof setTimeout>) => void;
}

export interface AppLoadRetry {
	start(): void;
	retryNow(): void;
}

function isRetryableLoadError(errorCode: number): boolean {
	return errorCode < 0 && errorCode !== ERR_ABORTED;
}

function getLoadErrorCode(error: unknown): number | null {
	const message = error instanceof Error ? error.message : String(error);
	const match = /\(([-\d]+)\)/.exec(message);
	if (!match) return null;
	const value = Number.parseInt(match[1], 10);
	return Number.isFinite(value) ? value : null;
}

export function createAppLoadRetry(options: AppLoadRetryOptions): AppLoadRetry {
	const {webContents, logger} = options;
	const setTimer = options.setTimer ?? setTimeout;
	const clearTimer = options.clearTimer ?? clearTimeout;
	const appUrl = options.appUrl;
	let attempt = 0;
	let timer: ReturnType<typeof setTimeout> | null = null;
	const cancelTimer = () => {
		if (timer) {
			clearTimer(timer);
			timer = null;
		}
	};
	const load = (reason: string) => {
		if (webContents.isDestroyed()) return;
		webContents.loadURL(appUrl).catch((error: unknown) => {
			const errorCode = getLoadErrorCode(error);
			if (errorCode !== null && !isRetryableLoadError(errorCode)) {
				logger.info('Ignoring non-retryable app load rejection', {reason, errorCode});
				return;
			}
			schedule(`${reason}-rejected`, {error});
		});
	};
	const schedule = (reason: string, detail?: Record<string, unknown>) => {
		if (timer) return;
		const delay = Math.min(INITIAL_RETRY_DELAY_MS * 2 ** attempt, MAX_RETRY_DELAY_MS);
		attempt += 1;
		logger.warn('Scheduling app load retry', {reason, delay, attempt, ...detail});
		timer = setTimer(() => {
			timer = null;
			if (webContents.isDestroyed()) return;
			if (webContents.isLoadingMainFrame()) {
				schedule('main-frame-still-loading');
				return;
			}
			load('retry');
		}, delay);
	};
	const reset = () => {
		cancelTimer();
		attempt = 0;
	};
	return {
		start() {
			webContents.on('did-navigate', () => {
				reset();
				options.onCommitted();
			});
			webContents.on('did-fail-load', (_event, errorCode, errorDescription, validatedURL, isMainFrame) => {
				if (isMainFrame) {
					logger.error('App main-frame load failed', {errorCode, errorDescription, validatedURL});
				}
				if (!isMainFrame || !options.isTrustedUrl(validatedURL) || !isRetryableLoadError(errorCode)) {
					return;
				}
				schedule('did-fail-load', {errorCode, errorDescription, validatedURL});
				if (attempt >= FAILURES_BEFORE_PROMPT) {
					options.onRepeatedFailure({errorCode, errorDescription, url: validatedURL});
				}
			});
			logger.info('Loading app URL', {appUrl});
			load('initial-load');
		},
		retryNow() {
			reset();
			load('manual-retry');
		},
	};
}
