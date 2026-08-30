// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('PopoutWindowManager');

export const VOICE_POPOUT_WINDOW_NAME_PREFIX = 'fluxer-voice-popout:';
export const VOICE_POPOUTS_MAX = 8;
export const VOICE_TILE_POPOUT_DEFAULT_WIDTH = 854;
export const VOICE_TILE_POPOUT_DEFAULT_HEIGHT = 480;
export const VOICE_CALL_POPOUT_DEFAULT_WIDTH = 960;
export const VOICE_CALL_POPOUT_DEFAULT_HEIGHT = 600;

export type VoiceTilePopoutSource = 'user' | 'camera' | 'screen_share';

export interface VoiceTilePopoutDescriptor {
	kind: 'tile';
	key: string;
	participantIdentity: string;
	source: VoiceTilePopoutSource;
	userId: string;
	connectionId: string;
	channelId: string;
	guildId: string | null;
	title: string;
}

export interface VoiceCallPopoutDescriptor {
	kind: 'call';
	key: string;
	channelId: string;
	guildId: string | null;
	title: string;
}

export type VoicePopoutDescriptor = VoiceTilePopoutDescriptor | VoiceCallPopoutDescriptor;

export function getVoiceTilePopoutKey(participantIdentity: string, source: VoiceTilePopoutSource): string {
	return `${VOICE_POPOUT_WINDOW_NAME_PREFIX}tile:${source}:${participantIdentity}`;
}

export function getVoiceCallPopoutKey(channelId: string): string {
	return `${VOICE_POPOUT_WINDOW_NAME_PREFIX}call:${channelId}`;
}

export function isVoicePopoutSupported(): boolean {
	const electronApi = getElectronAPI();
	if (!electronApi) return false;
	return typeof electronApi.popoutSetAlwaysOnTop === 'function';
}

interface PopoutChildWindow {
	closed: boolean;
	focus: () => void;
	close: () => void;
}

class PopoutWindowManagerStore {
	popouts: Record<string, VoicePopoutDescriptor> = {};
	alwaysOnTopKeys: Record<string, true> = {};
	private readonly childWindows = new Map<string, PopoutChildWindow>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get openPopouts(): Array<VoicePopoutDescriptor> {
		return Object.values(this.popouts);
	}

	get openPopoutCount(): number {
		let count = 0;
		for (const key in this.popouts) {
			if (this.popouts[key]) {
				count += 1;
			}
		}
		return count;
	}

	get callPopout(): VoiceCallPopoutDescriptor | null {
		for (const key in this.popouts) {
			const popout = this.popouts[key];
			if (!popout) continue;
			if (popout.kind === 'call') return popout;
		}
		return null;
	}

	isOpen(key: string): boolean {
		return key in this.popouts;
	}

	isCallPopoutOpenForChannel(channelId: string): boolean {
		return this.callPopout?.channelId === channelId;
	}

	isAlwaysOnTop(key: string): boolean {
		return this.alwaysOnTopKeys[key] === true;
	}

	openTilePopout(options: Omit<VoiceTilePopoutDescriptor, 'kind' | 'key'>): boolean {
		const key = getVoiceTilePopoutKey(options.participantIdentity, options.source);
		return this.register({kind: 'tile', key, ...options});
	}

	openCallPopout(options: Omit<VoiceCallPopoutDescriptor, 'kind' | 'key'>): boolean {
		const existingCallPopout = this.callPopout;
		const key = getVoiceCallPopoutKey(options.channelId);
		if (existingCallPopout && existingCallPopout.key !== key) {
			this.close(existingCallPopout.key);
		}
		return this.register({kind: 'call', key, ...options});
	}

	focus(key: string): void {
		if (!this.isOpen(key)) return;
		const electronApi = getElectronAPI();
		void electronApi?.popoutFocus?.(key).catch((error) => {
			logger.warn('Failed to focus popout window via desktop API', {key, error});
		});
		const childWindow = this.childWindows.get(key);
		if (childWindow && !childWindow.closed) {
			childWindow.focus();
		}
	}

	attachWindow(key: string, childWindow: PopoutChildWindow | null): void {
		if (!this.isOpen(key)) return;
		if (childWindow === null) {
			this.childWindows.delete(key);
			return;
		}
		this.childWindows.set(key, childWindow);
	}

	setAlwaysOnTop(key: string, flag: boolean): void {
		if (!this.isOpen(key)) return;
		const electronApi = getElectronAPI();
		void electronApi?.popoutSetAlwaysOnTop?.(key, flag).catch((error) => {
			logger.warn('Failed to toggle popout always-on-top', {key, flag, error});
		});
		runInAction(() => {
			if (flag) {
				this.alwaysOnTopKeys = {...this.alwaysOnTopKeys, [key]: true};
			} else {
				const next = {...this.alwaysOnTopKeys};
				delete next[key];
				this.alwaysOnTopKeys = next;
			}
		});
	}

	toggleAlwaysOnTop(key: string): void {
		this.setAlwaysOnTop(key, !this.isAlwaysOnTop(key));
	}

	handleWindowClosed(key: string): void {
		this.childWindows.delete(key);
		this.remove(key);
	}

	close(key: string): void {
		const childWindow = this.childWindows.get(key);
		this.childWindows.delete(key);
		this.remove(key);
		if (childWindow && !childWindow.closed) {
			childWindow.close();
		}
	}

	closeAll(): void {
		for (const key of Object.keys(this.popouts)) {
			this.close(key);
		}
	}

	private register(descriptor: VoicePopoutDescriptor): boolean {
		if (this.isOpen(descriptor.key)) {
			this.focus(descriptor.key);
			return true;
		}
		if (!isVoicePopoutSupported()) {
			logger.warn('Ignored popout request: desktop popout API unavailable', {key: descriptor.key});
			return false;
		}
		if (this.openPopoutCount >= VOICE_POPOUTS_MAX) {
			logger.warn('Ignored popout request: popout capacity reached', {key: descriptor.key});
			return false;
		}
		runInAction(() => {
			this.popouts = {...this.popouts, [descriptor.key]: descriptor};
		});
		return true;
	}

	private remove(key: string): void {
		if (!this.isOpen(key)) return;
		runInAction(() => {
			const nextPopouts = {...this.popouts};
			delete nextPopouts[key];
			this.popouts = nextPopouts;
			const nextAlwaysOnTop = {...this.alwaysOnTopKeys};
			delete nextAlwaysOnTop[key];
			this.alwaysOnTopKeys = nextAlwaysOnTop;
		});
	}
}

export default new PopoutWindowManagerStore();
