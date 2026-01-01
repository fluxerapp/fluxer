/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable} from 'mobx';
import {Logger} from '~/lib/Logger';

const logger = new Logger('WindowStore');

interface WindowSize {
	width: number;
	height: number;
}

const getWindowSize = (): WindowSize => ({
	width: window.innerWidth,
	height: window.innerHeight,
});

const getInitialFocused = (): boolean => document.hasFocus();

const getInitialVisible = (): boolean => !document.hidden;

function generateWindowId(): string {
	return `window-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

class WindowStore {
	focused = getInitialFocused();
	visible = getInitialVisible();
	windowSize: WindowSize = getWindowSize();
	windowId: string = generateWindowId();
	lastFocusedAt: number = Date.now();
	createdAt: number = Date.now();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initListeners();
	}

	private initListeners(): void {
		window.addEventListener('focus', () => this.setFocused(true));
		window.addEventListener('blur', () => this.setFocused(false));
		document.addEventListener('visibilitychange', () => {
			this.setVisible(!document.hidden);
		});
		window.addEventListener('resize', () => this.updateWindowSize());
	}

	setFocused(focused: boolean): void {
		if (this.focused !== focused) {
			logger.debug(`Window focus changed: ${focused}`);
			this.focused = focused;
			if (focused) {
				this.lastFocusedAt = Date.now();
			}
		}
	}

	setVisible(visible: boolean): void {
		if (this.visible !== visible) {
			logger.debug(`Window visibility changed: ${visible}`);
			this.visible = visible;
		}
	}

	updateWindowSize(): void {
		this.windowSize = getWindowSize();
		logger.debug(`Window resized: ${this.windowSize.width}x${this.windowSize.height}`);
	}

	isFocused(): boolean {
		return this.focused;
	}

	isVisible(): boolean {
		return this.visible;
	}

	getWindowSize(): WindowSize {
		return this.windowSize;
	}
}

export default new WindowStore();
