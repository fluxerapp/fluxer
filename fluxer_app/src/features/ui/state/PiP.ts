// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {makeAutoObservable, runInAction} from 'mobx';

type PiPContentType = 'stream';
type PiPCorner = 'top-left' | 'top-right' | 'bottom-right' | 'bottom-left';

const PIP_DEFAULT_WIDTH = 320;

interface PiPContent {
	type: PiPContentType;
	participantIdentity: string;
	channelId: string;
	guildId: string | null;
	connectionId: string;
	userId: string;
}

const PIP_CORNER_STORAGE_KEY = 'pip_corner';
const PIP_WIDTH_STORAGE_KEY = 'pip_width';
const PIP_CORNERS: ReadonlyArray<PiPCorner> = ['top-left', 'top-right', 'bottom-right', 'bottom-left'];
const logger = new Logger('PiP');

function isPiPCorner(value: string | null): value is PiPCorner {
	if (!value) return false;
	return PIP_CORNERS.includes(value as PiPCorner);
}

function parsePiPWidth(value: string | null): number | null {
	if (!value) return null;
	const parsed = Number(value);
	if (!Number.isFinite(parsed) || parsed <= 0) return null;
	return parsed;
}

class PiP {
	isOpen = false;
	content: PiPContent | null = null;
	corner: PiPCorner = 'bottom-right';
	sessionDisable = false;
	width = PIP_DEFAULT_WIDTH;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		const storedCorner = AppStorage.getItem(PIP_CORNER_STORAGE_KEY);
		if (isPiPCorner(storedCorner)) {
			this.corner = storedCorner;
		}
		const storedWidth = parsePiPWidth(AppStorage.getItem(PIP_WIDTH_STORAGE_KEY));
		if (storedWidth != null) {
			this.width = storedWidth;
		}
	}

	open(content: PiPContent): void {
		logger.debug('Opening PiP', {previousContent: this.content, content});
		runInAction(() => {
			this.isOpen = true;
			this.content = content;
		});
	}

	close(): void {
		logger.debug('Closing PiP', {previousContent: this.content, wasOpen: this.isOpen});
		runInAction(() => {
			this.isOpen = false;
			this.content = null;
		});
	}

	clearForChannel(channelId: string): void {
		logger.debug('Clearing PiP for channel', {
			channelId,
			previousContent: this.content,
			wasOpen: this.isOpen,
			willClear: this.content?.channelId === channelId,
		});
		runInAction(() => {
			if (this.content?.channelId === channelId) {
				this.isOpen = false;
				this.content = null;
			}
		});
	}

	setSessionDisable(value: boolean): void {
		logger.debug('Setting PiP session disable', {previousValue: this.sessionDisable, value});
		runInAction(() => {
			this.sessionDisable = value;
		});
	}

	setCorner(corner: PiPCorner): void {
		runInAction(() => {
			this.corner = corner;
		});
		AppStorage.setItem(PIP_CORNER_STORAGE_KEY, corner);
	}

	setWidth(width: number): void {
		runInAction(() => {
			this.width = width;
		});
		AppStorage.setItem(PIP_WIDTH_STORAGE_KEY, `${width}`);
	}

	getContent(): PiPContent | null {
		return this.content;
	}

	getIsOpen(): boolean {
		return this.isOpen;
	}

	getCorner(): PiPCorner {
		return this.corner;
	}

	getEffectiveCorner(): PiPCorner {
		return this.corner;
	}

	getSessionDisable(): boolean {
		return this.sessionDisable;
	}

	getWidth(): number {
		return this.width;
	}
}

export type {PiPContent, PiPContentType, PiPCorner};
export {PIP_DEFAULT_WIDTH};

export default new PiP();
