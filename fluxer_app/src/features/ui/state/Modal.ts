// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getActivePortalHost, type PortalHostElement} from '@app/features/ui/overlay/PortalHostContext';
import type {ModalRender} from '@app/features/ui/state/ModalRender';
import Toast from '@app/features/ui/state/Toast';
import {shouldRestoreFocusToTarget} from '@app/features/ui/utils/PointerActivationFocus';
import {i18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {makeAutoObservable} from 'mobx';
import type React from 'react';

const HAVING_FUN_DESCRIPTOR = msg({
	message: 'Having fun?',
	comment: 'Friendly tagline shown on a Fluxer-branded button or upsell.',
});
const logger = new Logger('Modal');

type KeyboardModeStateResolver = () => boolean;
type KeyboardModeRestoreCallback = (showIntro: boolean) => void;

let keyboardModeStateResolver: KeyboardModeStateResolver | undefined;
let keyboardModeRestoreCallback: KeyboardModeRestoreCallback | undefined;

export function registerKeyboardModeStateResolver(resolver: KeyboardModeStateResolver): void {
	keyboardModeStateResolver = resolver;
}

export function registerKeyboardModeRestoreCallback(callback: KeyboardModeRestoreCallback): void {
	keyboardModeRestoreCallback = callback;
}

const BASE_Z_INDEX = 10000;
const Z_INDEX_INCREMENT = 2;

export function getZIndexForStack(stackIndex: number): number {
	return BASE_Z_INDEX + stackIndex * Z_INDEX_INCREMENT;
}

export function getBackdropZIndexForStack(stackIndex: number): number {
	return BASE_Z_INDEX + stackIndex * Z_INDEX_INCREMENT - 1;
}

interface ModalEntry {
	modal: ModalRender;
	key: string;
	focusReturnTarget: HTMLElement | null;
	keyboardModeEnabled: boolean;
	restoreFocusOnClose: boolean;
	isBackground: boolean;
	ownerDocument: Document;
	portalHost: PortalHostElement;
}

interface ModalWithStackInfo extends ModalEntry {
	stackIndex: number;
	isVisible: boolean;
	needsBackdrop: boolean;
	isTopmost: boolean;
}

interface PushOptions {
	isBackground?: boolean;
	forceMainWindow?: boolean;
	portalHost?: PortalHostElement;
}

class ModalState {
	modals: Array<ModalEntry> = [];
	private hasShownStackingToast = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	push(modal: ModalRender, key: string | number, options: PushOptions = {}): void {
		const isBackground = options.isBackground ?? false;
		const keyboardModeEnabled = keyboardModeStateResolver ? keyboardModeStateResolver() : false;
		const portalHost = this.resolvePortalHost(options);
		const ownerDocument = this.resolveOwnerDocument(portalHost);
		const focusReturnTarget = this.getActiveElement(ownerDocument);
		const restoreFocusOnClose = shouldRestoreFocusToTarget(focusReturnTarget, keyboardModeEnabled);
		this.modals.push({
			modal,
			key: key.toString(),
			focusReturnTarget: restoreFocusOnClose ? focusReturnTarget : null,
			keyboardModeEnabled,
			restoreFocusOnClose,
			isBackground,
			ownerDocument,
			portalHost,
		});
		this.checkAlternatingStackPattern();
	}

	private getModalSignature(modal: ModalEntry): string {
		const element = modal.modal();
		const typeName = typeof element.type === 'function' ? element.type.name : String(element.type);
		try {
			return `${typeName}:${JSON.stringify(element.props)}`;
		} catch {
			return `${typeName}:${modal.key}`;
		}
	}

	private checkAlternatingStackPattern(): void {
		if (this.hasShownStackingToast) return;
		if (this.modals.length < 5) return;
		const lastFive = this.modals.slice(-5);
		const signatures = lastFive.map((m) => this.getModalSignature(m));
		const signatureA = signatures[0];
		const signatureB = signatures[1];
		if (signatureA === signatureB) return;
		const isAlternating = signatures[2] === signatureA && signatures[3] === signatureB && signatures[4] === signatureA;
		if (isAlternating) {
			this.hasShownStackingToast = true;
			Toast.createToast({type: 'info', children: i18n._(HAVING_FUN_DESCRIPTOR), timeout: 3000});
		}
	}

	update(key: string | number, updater: (currentModal: ModalRender) => ModalRender, options?: PushOptions): void {
		const modalIndex = this.modals.findIndex((modal) => modal.key === key.toString());
		if (modalIndex === -1) return;
		const existingModal = this.modals[modalIndex];
		const shouldUpdateOwner = options?.forceMainWindow === true || (options ? 'portalHost' in options : false);
		const portalHost = shouldUpdateOwner && options ? this.resolvePortalHost(options) : existingModal.portalHost;
		const ownerDocument = shouldUpdateOwner ? this.resolveOwnerDocument(portalHost) : existingModal.ownerDocument;
		this.modals[modalIndex] = {
			...existingModal,
			modal: updater(existingModal.modal),
			isBackground: options?.isBackground ?? existingModal.isBackground,
			ownerDocument,
			portalHost,
		};
	}

	pop(key?: string | number, ownerDocument?: Document): void {
		let removed: ModalEntry | undefined;
		let wasTopmost = false;
		if (key) {
			const keyStr = key.toString();
			const idx = this.modals.findIndex((modal) => modal.key === keyStr);
			if (idx !== -1) {
				wasTopmost = idx === this.modals.length - 1;
				[removed] = this.modals.splice(idx, 1);
			}
		} else {
			const modalIndex = this.findTopModalIndex(ownerDocument ?? document);
			if (modalIndex !== -1) {
				wasTopmost = modalIndex === this.modals.length - 1;
				[removed] = this.modals.splice(modalIndex, 1);
			}
		}
		if (removed && wasTopmost) {
			logger.debug(`Modal.pop restoring focus topmost=${wasTopmost} keyboardMode=${removed.keyboardModeEnabled}`);
			this.scheduleFocus(removed.focusReturnTarget, removed.keyboardModeEnabled);
		}
	}

	popAll(): void {
		const lastModal = this.modals.at(-1);
		this.modals = [];
		if (lastModal) {
			this.scheduleFocus(lastModal.focusReturnTarget, lastModal.keyboardModeEnabled);
		}
	}

	popByType<T>(component: React.ComponentType<T>, ownerDocument?: Document): void {
		const modalIndex = this.modals.findLastIndex((modal) => {
			if (ownerDocument && modal.ownerDocument !== ownerDocument) return false;
			return modal.modal().type === component;
		});
		if (modalIndex === -1) return;
		const wasTopmost = modalIndex === this.modals.length - 1;
		const [removed] = this.modals.splice(modalIndex, 1);
		if (removed && wasTopmost) {
			logger.debug(`Modal.popByType restoring focus topmost=${wasTopmost} keyboardMode=${removed.keyboardModeEnabled}`);
			this.scheduleFocus(removed.focusReturnTarget, removed.keyboardModeEnabled);
		}
	}

	get orderedModals(): Array<ModalWithStackInfo> {
		return this.getOrderedModals(document);
	}

	getOrderedModals(ownerDocument: Document = document): Array<ModalWithStackInfo> {
		const modals = this.modals.filter((modal) => modal.ownerDocument === ownerDocument);
		const topmostRegularIndex = modals.findLastIndex((m) => !m.isBackground);
		const topmostIndex = modals.length - 1;
		return modals.map((modal, index) => {
			const isVisible = modal.isBackground || index === topmostRegularIndex;
			const needsBackdrop = modal.isBackground || (!modal.isBackground && index === topmostRegularIndex);
			return {
				...modal,
				stackIndex: index,
				isVisible,
				needsBackdrop,
				isTopmost: index === topmostIndex,
			};
		});
	}

	getKeyAtStackIndex(stackIndex: number, ownerDocument: Document = document): string | undefined {
		return this.modals.filter((modal) => modal.ownerDocument === ownerDocument)[stackIndex]?.key;
	}

	getModal(ownerDocument: Document = document): ModalEntry | undefined {
		return this.modals.findLast((modal) => modal.ownerDocument === ownerDocument);
	}

	hasModalOpen(ownerDocument: Document = document): boolean {
		return this.modals.some((modal) => modal.ownerDocument === ownerDocument);
	}

	hasModal(key: string): boolean {
		return this.modals.some((modal) => modal.key === key);
	}

	hasModalOfType<T>(component: React.ComponentType<T>, ownerDocument?: Document): boolean {
		return this.modals.some((modal) => {
			if (ownerDocument && modal.ownerDocument !== ownerDocument) return false;
			return modal.modal().type === component;
		});
	}

	private resolvePortalHost(options: PushOptions): PortalHostElement {
		if (options.forceMainWindow) return null;
		if ('portalHost' in options) return options.portalHost ?? null;
		return getActivePortalHost();
	}

	private resolveOwnerDocument(portalHost: PortalHostElement): Document {
		return portalHost?.ownerDocument ?? document;
	}

	private findTopModalIndex(ownerDocument: Document): number {
		for (let index = this.modals.length - 1; index >= 0; index--) {
			if (this.modals[index]?.ownerDocument === ownerDocument) {
				return index;
			}
		}
		return -1;
	}

	private getActiveElement(ownerDocument: Document): HTMLElement | null {
		const active = ownerDocument.activeElement;
		if (!active) return null;
		const activeDocument = active.ownerDocument ?? ownerDocument;
		const activeWindow = activeDocument.defaultView;
		if (activeWindow && active instanceof activeWindow.HTMLElement) {
			return active as HTMLElement;
		}
		return active instanceof HTMLElement ? active : null;
	}

	private scheduleFocus(target: HTMLElement | null, keyboardModeEnabled: boolean): void {
		logger.debug(`Modal.scheduleFocus target=${target ? target.tagName : 'null'} keyboardMode=${keyboardModeEnabled}`);
		if (!target) return;
		queueMicrotask(() => {
			if (!target.isConnected) {
				logger.debug('Modal.scheduleFocus aborted: target disconnected');
				return;
			}
			try {
				target.focus({preventScroll: true});
				logger.debug('Modal.scheduleFocus applied focus to target');
			} catch (error) {
				logger.error('Modal.scheduleFocus failed to focus target', error as Error);
				return;
			}
			if (keyboardModeEnabled && keyboardModeRestoreCallback) {
				logger.debug('Modal.scheduleFocus re-entering keyboard mode');
				keyboardModeRestoreCallback(false);
			}
		});
	}
}

export default new ModalState();
