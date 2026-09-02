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
	generation: number;
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
	generation: number;
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
	if (electronApi) return typeof electronApi.popoutSetAlwaysOnTop === 'function';
	return typeof window !== 'undefined' && typeof window.open === 'function';
}

export function isVoicePopoutAlwaysOnTopSupported(): boolean {
	return typeof getElectronAPI()?.popoutSetAlwaysOnTop === 'function';
}

interface PopoutChildWindow {
	closed: boolean;
	focus: () => void;
	close: () => void;
}

interface AlwaysOnTopOperationState {
	readonly descriptor: VoicePopoutDescriptor;
	desired: boolean;
}

class PopoutWindowManagerStore {
	popouts: Record<string, VoicePopoutDescriptor> = {};
	alwaysOnTopKeys: Record<string, true> = {};
	private readonly childWindows = new Map<string, PopoutChildWindow>();
	private readonly alwaysOnTopOperations = new Map<string, AlwaysOnTopOperationState>();
	private nextPopoutGeneration = 1;

	constructor() {
		makeAutoObservable<this, 'alwaysOnTopOperations' | 'childWindows' | 'nextPopoutGeneration'>(
			this,
			{
				alwaysOnTopOperations: false,
				childWindows: false,
				nextPopoutGeneration: false,
			},
			{autoBind: true},
		);
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

	openTilePopout(options: Omit<VoiceTilePopoutDescriptor, 'kind' | 'key' | 'generation'>): boolean {
		const key = getVoiceTilePopoutKey(options.participantIdentity, options.source);
		return this.register({kind: 'tile', key, generation: this.allocatePopoutGeneration(), ...options});
	}

	openCallPopout(options: Omit<VoiceCallPopoutDescriptor, 'kind' | 'key' | 'generation'>): boolean {
		const existingCallPopout = this.callPopout;
		const key = getVoiceCallPopoutKey(options.channelId);
		if (existingCallPopout && existingCallPopout.key !== key) {
			this.close(existingCallPopout.key);
		}
		return this.register({kind: 'call', key, generation: this.allocatePopoutGeneration(), ...options});
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

	attachWindow(key: string, generation: number, childWindow: PopoutChildWindow | null): void {
		if (!this.isCurrentPopout(key, generation)) return;
		if (childWindow === null) {
			this.childWindows.delete(key);
			return;
		}
		this.childWindows.set(key, childWindow);
	}

	setAlwaysOnTop(key: string, flag: boolean): void {
		const descriptor = this.popouts[key];
		if (!descriptor) return;
		if (!isVoicePopoutAlwaysOnTopSupported()) {
			logger.warn('Ignored always-on-top request: desktop popout API unavailable', {key, flag});
			return;
		}
		const currentOperation = this.alwaysOnTopOperations.get(key);
		if (currentOperation?.descriptor === descriptor) {
			currentOperation.desired = flag;
			return;
		}
		const operationState: AlwaysOnTopOperationState = {descriptor, desired: flag};
		this.alwaysOnTopOperations.set(key, operationState);
		void this.applyAlwaysOnTopOperation(key, operationState);
	}

	toggleAlwaysOnTop(key: string): void {
		const descriptor = this.popouts[key];
		if (!descriptor) return;
		const currentOperation = this.alwaysOnTopOperations.get(key);
		const currentFlag =
			currentOperation?.descriptor === descriptor ? currentOperation.desired : this.isAlwaysOnTop(key);
		this.setAlwaysOnTop(key, !currentFlag);
	}

	handleWindowClosed(key: string, generation: number): void {
		if (!this.isCurrentPopout(key, generation)) return;
		this.childWindows.delete(key);
		this.remove(key);
	}

	close(key: string, generation?: number): void {
		if (generation !== undefined && !this.isCurrentPopout(key, generation)) return;
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

	closeConnectionBoundPopouts(): void {
		for (const descriptor of this.openPopouts) {
			if (descriptor.kind === 'tile' && descriptor.source === 'user') continue;
			this.close(descriptor.key);
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
		if (!getElectronAPI()) {
			const width = descriptor.kind === 'call' ? VOICE_CALL_POPOUT_DEFAULT_WIDTH : VOICE_TILE_POPOUT_DEFAULT_WIDTH;
			const height = descriptor.kind === 'call' ? VOICE_CALL_POPOUT_DEFAULT_HEIGHT : VOICE_TILE_POPOUT_DEFAULT_HEIGHT;
			const childWindow = window.open('about:blank', descriptor.key, `width=${width},height=${height}`);
			if (!childWindow) {
				logger.warn('Failed to open browser popout window', {key: descriptor.key});
				return false;
			}
			this.childWindows.set(descriptor.key, childWindow);
		}
		runInAction(() => {
			this.popouts = {...this.popouts, [descriptor.key]: descriptor};
		});
		return true;
	}

	private allocatePopoutGeneration(): number {
		const generation = this.nextPopoutGeneration;
		if (!Number.isSafeInteger(generation)) {
			throw new Error('Voice popout generation exhausted');
		}
		this.nextPopoutGeneration += 1;
		return generation;
	}

	private isCurrentPopout(key: string, generation: number): boolean {
		return this.popouts[key]?.generation === generation;
	}

	private async applyAlwaysOnTopOperation(key: string, operationState: AlwaysOnTopOperationState): Promise<void> {
		while (this.alwaysOnTopOperations.get(key) === operationState) {
			const {descriptor} = operationState;
			if (!this.isCurrentPopout(key, descriptor.generation)) return;
			const flag = operationState.desired;
			const operation = getElectronAPI()?.popoutSetAlwaysOnTop?.(key, flag);
			if (!operation) {
				this.alwaysOnTopOperations.delete(key);
				return;
			}
			try {
				await operation;
			} catch (error) {
				if (
					this.alwaysOnTopOperations.get(key) !== operationState ||
					!this.isCurrentPopout(key, descriptor.generation)
				) {
					return;
				}
				logger.warn('Failed to toggle popout always-on-top', {key, flag, error});
				if (operationState.desired === flag) {
					this.alwaysOnTopOperations.delete(key);
					return;
				}
				continue;
			}
			if (this.alwaysOnTopOperations.get(key) !== operationState || !this.isCurrentPopout(key, descriptor.generation)) {
				return;
			}
			runInAction(() => {
				if (flag) {
					this.alwaysOnTopKeys = {...this.alwaysOnTopKeys, [key]: true};
				} else {
					const next = {...this.alwaysOnTopKeys};
					delete next[key];
					this.alwaysOnTopKeys = next;
				}
			});
			if (operationState.desired === flag) {
				this.alwaysOnTopOperations.delete(key);
				return;
			}
		}
	}

	private remove(key: string): void {
		if (!this.isOpen(key)) return;
		this.alwaysOnTopOperations.delete(key);
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
