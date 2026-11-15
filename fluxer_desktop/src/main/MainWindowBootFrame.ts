// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {hasQuitStarted} from '@electron/main/ModuleBootHandoff';
import type {DesktopBootFrame} from '@fluxer/desktop_ipc/src/BootStateContract';
import {app, type BrowserWindow, ImageView, nativeImage, nativeTheme, type WebContents} from 'electron';

const logger = createChildLogger('MainWindowBootFrame');
const NATIVE_FRAME_MAX_HOLD_MS = 10_000;

type FrameViewport = Pick<DesktopBootFrame, 'viewportWidth' | 'viewportHeight' | 'systemDark'>;

export class MainWindowBootFrame {
	private readonly window: BrowserWindow;
	private readonly contents: WebContents;
	private readonly onDeadline: (late: boolean) => void;
	private readonly deadlineMs: number;
	private preparation: Promise<void> | null = null;
	private frame: DesktopBootFrame | null = null;
	private viewport: FrameViewport | null = null;
	private view: ImageView | null = null;
	private holdTimer: NodeJS.Timeout | null = null;
	private domFramePainted = false;
	private deadlineTimer: NodeJS.Timeout | null = null;
	private settled = false;
	private initialNavigationPending = true;
	private fallbackReady = false;

	public constructor(window: BrowserWindow, deadlineMs: number, onDeadline: (late: boolean) => void) {
		this.window = window;
		this.contents = window.webContents;
		this.deadlineMs = deadlineMs;
		this.onDeadline = onDeadline;
		window.on('resize', this.checkViewport);
		window.on('close', this.dispose);
		window.once('closed', this.dispose);
		window.webContents.on('render-process-gone', this.dispose);
		window.webContents.on('did-start-navigation', this.onNavigation);
		nativeTheme.on('updated', this.checkViewport);
		app.once('before-quit', this.dispose);
	}

	public prepare(readFrame: () => Promise<DesktopBootFrame | null>): Promise<void> {
		if (this.settled) return Promise.resolve();
		if (this.preparation !== null) return this.preparation;
		this.preparation = readFrame()
			.then((frame) => {
				if (this.settled || hasQuitStarted() || this.window.isDestroyed() || frame === null) return;
				if (!this.matchesViewport(frame)) return;
				this.frame = frame;
				this.viewport = {
					viewportWidth: frame.viewportWidth,
					viewportHeight: frame.viewportHeight,
					systemDark: frame.systemDark,
				};
				const [width, height] = this.window.getContentSize();
				const scaleFactor = frame.pixelWidth / frame.viewportWidth;
				const image = nativeImage.createFromBuffer(Buffer.from(frame.jpeg), {scaleFactor});
				if (image.isEmpty()) return;
				const view = new ImageView();
				view.setImage(image.resize({width, height, quality: 'best'}));
				view.setBounds({x: 0, y: 0, width, height});
				this.view = view;
				this.window.contentView.addChildView(view);
				this.fallbackReady = true;
				this.holdTimer = setTimeout(this.dispose, NATIVE_FRAME_MAX_HOLD_MS);
				const waitMs = this.deadlineMs - process.uptime() * 1000;
				if (waitMs <= 0) {
					this.revealNativeFrame(true);
				} else {
					this.deadlineTimer = setTimeout(() => {
						this.deadlineTimer = null;
						this.revealNativeFrame(false);
					}, waitMs);
				}
			})
			.catch((error: unknown) => {
				logger.warn('Failed to prepare the native boot frame', {error});
				this.dispose();
			});
		return this.preparation;
	}

	public async takeFrame(): Promise<DesktopBootFrame | null> {
		await this.preparation;
		this.checkViewport();
		if (this.settled) return null;
		const frame = this.frame;
		this.frame = null;
		return frame;
	}

	public markDOMFramePainted(): boolean {
		this.domFramePainted = true;
		this.checkViewport();
		if (this.deadlineTimer === null) this.removeNativeView();
		return this.fallbackReady;
	}

	public readonly dispose = (): void => {
		this.settled = true;
		this.fallbackReady = false;
		this.frame = null;
		this.viewport = null;
		if (this.holdTimer !== null) {
			clearTimeout(this.holdTimer);
			this.holdTimer = null;
		}
		if (this.deadlineTimer !== null) {
			clearTimeout(this.deadlineTimer);
			this.deadlineTimer = null;
		}
		this.removeNativeView();
		this.window.removeListener('resize', this.checkViewport);
		this.window.removeListener('close', this.dispose);
		this.window.removeListener('closed', this.dispose);
		this.contents.removeListener('render-process-gone', this.dispose);
		this.contents.removeListener('did-start-navigation', this.onNavigation);
		nativeTheme.removeListener('updated', this.checkViewport);
		app.removeListener('before-quit', this.dispose);
	};

	private readonly onNavigation = (
		_event: Electron.Event,
		_url: string,
		sameDocument: boolean,
		mainFrame: boolean,
	): void => {
		if (!mainFrame || sameDocument) return;
		if (this.initialNavigationPending) {
			this.initialNavigationPending = false;
			return;
		}
		this.dispose();
	};

	private readonly checkViewport = (): void => {
		if (this.viewport !== null && !this.matchesViewport(this.viewport)) this.dispose();
	};

	private matchesViewport(frame: FrameViewport): boolean {
		if (this.window.isDestroyed()) return false;
		const [width, height] = this.window.getContentSize();
		return (
			Math.abs(width - frame.viewportWidth) <= 1 &&
			Math.abs(height - frame.viewportHeight) <= 1 &&
			(frame.systemDark === null || nativeTheme.shouldUseDarkColors === frame.systemDark)
		);
	}

	private revealNativeFrame(late: boolean): void {
		this.checkViewport();
		if (!this.settled && this.fallbackReady && !hasQuitStarted()) this.onDeadline(late);
		if (this.domFramePainted) this.removeNativeView();
	}

	private removeNativeView(): void {
		const view = this.view;
		if (view === null) return;
		this.view = null;
		if (!this.window.isDestroyed()) this.window.contentView.removeChildView(view);
		view.setImage(nativeImage.createEmpty());
	}
}
