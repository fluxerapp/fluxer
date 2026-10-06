// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {shouldRestoreFocusToTarget} from '@app/features/ui/utils/PointerActivationFocus';
import {makeAutoObservable, observableRef} from 'mobx';
import type React from 'react';

const logger = new Logger('ContextMenu');

export const CONTEXT_MENU_OPEN_ATTRIBUTE = 'data-context-menu-open';

export interface FocusableContextMenuTarget {
	tagName: string;
	isConnected: boolean;
	focus: (options?: FocusOptions) => void;
	addEventListener: HTMLElement['addEventListener'];
	removeEventListener: HTMLElement['removeEventListener'];
}

export type ContextMenuTargetElement = HTMLElement | FocusableContextMenuTarget;

export function isContextMenuNodeTarget(target: ContextMenuTargetElement | null | undefined): target is HTMLElement {
	if (!target || typeof Node === 'undefined') {
		return false;
	}
	const ownerDocument = (target as HTMLElement).ownerDocument;
	const ownerWindow = ownerDocument?.defaultView;
	if (ownerWindow) {
		return target instanceof ownerWindow.HTMLElement;
	}
	return target instanceof HTMLElement;
}

export interface ContextMenuTarget {
	x: number;
	y: number;
	target: ContextMenuTargetElement;
}

export interface ContextMenuConfig {
	onClose?: () => void;
	onBackdropMouseDown?: (event: React.MouseEvent<HTMLDivElement>) => boolean | undefined;
	noBlurEvent?: boolean;
	returnFocus?: boolean;
	returnFocusTarget?: ContextMenuTargetElement | null;
	align?: 'top-left' | 'top-right' | 'bottom-left' | 'bottom-right';
	trackDynamicPosition?: boolean;
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
	restoreFocus: boolean;
}

interface StackEntry {
	contextMenu: ContextMenu;
	focusRestoreState: FocusRestoreState | null;
}

interface CloseEntryOptions {
	restoreFocus?: boolean;
}

class ContextMenuState {
	private currentEntry: StackEntry | null = null;

	constructor() {
		makeAutoObservable<this, 'currentEntry'>(this, {currentEntry: observableRef}, {autoBind: true});
	}

	get contextMenu(): ContextMenu | null {
		return this.currentEntry?.contextMenu ?? null;
	}

	get contextMenus(): Array<ContextMenu> {
		return this.currentEntry ? [this.currentEntry.contextMenu] : [];
	}

	getContextMenu(ownerDocument: Document = document): ContextMenu | null {
		const contextMenu = this.contextMenu;
		if (!contextMenu) return null;
		const target = contextMenu.target.target;
		if (isContextMenuNodeTarget(target)) {
			return target.ownerDocument === ownerDocument ? contextMenu : null;
		}
		return ownerDocument === document ? contextMenu : null;
	}

	open(contextMenu: ContextMenu): void {
		logger.debug(`Opening context menu: ${contextMenu.id}`);
		const replacedEntry = this.currentEntry;
		this.currentEntry = this.createEntry(contextMenu);
		this.applyAnchorAttribute(contextMenu);
		if (replacedEntry) {
			logger.debug(`Replacing context menu: ${replacedEntry.contextMenu.id}`);
			this.closeEntry(replacedEntry, {restoreFocus: false});
		}
	}

	private createEntry(contextMenu: ContextMenu): StackEntry {
		const requestedTarget = contextMenu.config?.returnFocusTarget ?? contextMenu.target.target;
		const keyboardModeEnabled = KeyboardMode.keyboardModeEnabled;
		const restoreFocus = isContextMenuNodeTarget(requestedTarget)
			? shouldRestoreFocusToTarget(requestedTarget, keyboardModeEnabled)
			: true;
		return {
			contextMenu,
			focusRestoreState: {
				target: restoreFocus ? (requestedTarget ?? null) : null,
				keyboardModeEnabled,
				restoreFocus,
			},
		};
	}

	private applyAnchorAttribute(contextMenu: ContextMenu): void {
		const anchor = contextMenu.target.target;
		if (isContextMenuNodeTarget(anchor)) {
			anchor.setAttribute(CONTEXT_MENU_OPEN_ATTRIBUTE, 'true');
		}
	}

	private clearAnchorAttribute(contextMenu: ContextMenu): void {
		const anchor = contextMenu.target.target;
		if (!isContextMenuNodeTarget(anchor)) return;
		const currentAnchor = this.currentEntry?.contextMenu.target.target ?? null;
		if (anchor === currentAnchor) return;
		anchor.removeAttribute(CONTEXT_MENU_OPEN_ATTRIBUTE);
	}

	private closeEntry(entry: StackEntry, options: CloseEntryOptions = {}): void {
		const {contextMenu, focusRestoreState} = entry;
		const {config, target} = contextMenu;
		const shouldReturnFocus = (options.restoreFocus ?? true) && (config?.returnFocus ?? true);
		const fallbackTarget = target.target;
		const restoreState = shouldReturnFocus ? focusRestoreState : null;
		const focusTarget =
			restoreState?.restoreFocus === false
				? null
				: (config?.returnFocusTarget ?? restoreState?.target ?? fallbackTarget ?? null);
		const resumeKeyboardMode = Boolean(restoreState?.keyboardModeEnabled);
		this.clearAnchorAttribute(contextMenu);
		config?.onClose?.();
		if (shouldReturnFocus) {
			this.restoreFocus(focusTarget, resumeKeyboardMode);
		}
	}

	closeById(id: string): void {
		const entry = this.currentEntry;
		if (!entry || entry.contextMenu.id !== id) return;
		this.currentEntry = null;
		logger.debug(`Closing context menu by id: ${id}`);
		this.closeEntry(entry);
	}

	close(): void {
		const entry = this.currentEntry;
		if (!entry) return;
		this.currentEntry = null;
		logger.debug(`Closing context menu: ${entry.contextMenu.id}`);
		this.closeEntry(entry);
	}

	private restoreFocus(target: ContextMenuTargetElement | null, resumeKeyboardMode: boolean): void {
		logger.debug(
			`ContextMenu.restoreFocus target=${target ? target.tagName : 'null'} resumeKeyboardMode=${resumeKeyboardMode}`,
		);
		if (!target) return;
		queueMicrotask(() => {
			if (!target.isConnected) {
				logger.debug('ContextMenu.restoreFocus aborted: target disconnected');
				return;
			}
			try {
				target.focus({preventScroll: true});
				logger.debug('ContextMenu.restoreFocus applied focus to target');
			} catch (error) {
				logger.error('ContextMenu.restoreFocus failed to focus target', error as Error);
				return;
			}
			if (resumeKeyboardMode) {
				logger.debug('ContextMenu.restoreFocus re-entering keyboard mode');
				KeyboardMode.enterKeyboardMode(false);
			}
		});
	}
}

export default new ContextMenuState();
