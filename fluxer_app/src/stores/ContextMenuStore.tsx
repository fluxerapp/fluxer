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
import KeyboardModeStore from './KeyboardModeStore';

const logger = new Logger('ContextMenuStore');

export interface FocusableContextMenuTarget {
	tagName: string;
	isConnected: boolean;
	focus: (options?: FocusOptions) => void;
	addEventListener: HTMLElement['addEventListener'];
	removeEventListener: HTMLElement['removeEventListener'];
}

export type ContextMenuTargetElement = HTMLElement | FocusableContextMenuTarget;

export const isContextMenuNodeTarget = (target: ContextMenuTargetElement | null | undefined): target is HTMLElement => {
	if (!target || typeof Node === 'undefined') {
		return false;
	}
	return target instanceof HTMLElement;
};

export interface ContextMenuTarget {
	x: number;
	y: number;
	target: ContextMenuTargetElement;
}

export interface ContextMenuConfig {
	onClose?: () => void;
	noBlurEvent?: boolean;
	returnFocus?: boolean;
	returnFocusTarget?: ContextMenuTargetElement | null;
	align?: 'top-left' | 'top-right';
}

export interface ContextMenu {
	id: string;
	target: ContextMenuTarget;
	render: (props: {onClose: () => void}) => React.ReactNode;
	config?: ContextMenuConfig;
}

export interface FocusRestoreState {
	target: ContextMenuTargetElement | null;
	keyboardModeEnabled: boolean;
}

class ContextMenuStore {
	contextMenu: ContextMenu | null = null;
	private focusRestoreState: FocusRestoreState | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	open(contextMenu: ContextMenu): void {
		logger.debug(`Opening context menu: ${contextMenu.id}`);
		this.contextMenu = contextMenu;
		const requestedTarget = contextMenu.config?.returnFocusTarget ?? contextMenu.target.target;
		this.focusRestoreState = {
			target: requestedTarget ?? null,
			keyboardModeEnabled: KeyboardModeStore.keyboardModeEnabled,
		};
	}

	close(): void {
		if (this.contextMenu) {
			logger.debug(`Closing context menu: ${this.contextMenu.id}`);
			const {config, target} = this.contextMenu;
			const shouldReturnFocus = config?.returnFocus ?? true;
			const fallbackTarget = target.target;
			const restoreState = shouldReturnFocus ? this.focusRestoreState : null;
			const focusTarget = config?.returnFocusTarget ?? restoreState?.target ?? fallbackTarget ?? null;
			const resumeKeyboardMode = Boolean(restoreState?.keyboardModeEnabled);
			config?.onClose?.();
			this.contextMenu = null;
			this.focusRestoreState = null;
			if (shouldReturnFocus) {
				this.restoreFocus(focusTarget, resumeKeyboardMode);
			}
		}
	}

	private restoreFocus(target: ContextMenuTargetElement | null, resumeKeyboardMode: boolean): void {
		logger.debug(
			`ContextMenuStore.restoreFocus target=${target ? target.tagName : 'null'} resumeKeyboardMode=${resumeKeyboardMode}`,
		);
		if (!target) return;
		queueMicrotask(() => {
			if (!target.isConnected) {
				logger.debug('ContextMenuStore.restoreFocus aborted: target disconnected');
				return;
			}
			try {
				target.focus({preventScroll: true});
				logger.debug('ContextMenuStore.restoreFocus applied focus to target');
			} catch (error) {
				logger.error('ContextMenuStore.restoreFocus failed to focus target', error as Error);
				return;
			}
			if (resumeKeyboardMode) {
				logger.debug('ContextMenuStore.restoreFocus re-entering keyboard mode');
				KeyboardModeStore.enterKeyboardMode(false);
			}
		});
	}
}

export default new ContextMenuStore();
