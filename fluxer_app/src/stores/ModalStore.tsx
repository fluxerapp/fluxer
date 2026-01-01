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
import type React from 'react';
import type {ModalRender} from '~/actions/ModalActionCreators';
import {Logger} from '~/lib/Logger';
import KeyboardModeStore from './KeyboardModeStore';
import ToastStore from './ToastStore';

const logger = new Logger('ModalStore');

const BASE_Z_INDEX = 10000;
const Z_INDEX_INCREMENT = 2;

export function getZIndexForStack(stackIndex: number): number {
	return BASE_Z_INDEX + stackIndex * Z_INDEX_INCREMENT;
}

export function getBackdropZIndexForStack(stackIndex: number): number {
	return BASE_Z_INDEX + stackIndex * Z_INDEX_INCREMENT - 1;
}

interface Modal {
	modal: ModalRender;
	key: string;
	focusReturnTarget: HTMLElement | null;
	keyboardModeEnabled: boolean;
	isBackground: boolean;
}

interface ModalWithStackInfo extends Modal {
	stackIndex: number;
	isVisible: boolean;
	needsBackdrop: boolean;
}

interface PushOptions {
	isBackground?: boolean;
}

class ModalStore {
	modals: Array<Modal> = [];
	private hasShownStackingToast = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	push(modal: ModalRender, key: string | number, options: PushOptions = {}): void {
		const isBackground = options.isBackground ?? false;

		this.modals.push({
			modal,
			key: key.toString(),
			focusReturnTarget: this.getActiveElement(),
			keyboardModeEnabled: KeyboardModeStore.keyboardModeEnabled,
			isBackground,
		});

		this.checkAlternatingStackPattern();
	}

	private getModalSignature(modal: Modal): string {
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
			ToastStore.createToast({type: 'info', children: 'Having fun?', timeout: 3000});
		}
	}

	update(key: string | number, updater: (currentModal: ModalRender) => ModalRender, options?: PushOptions): void {
		const modalIndex = this.modals.findIndex((modal) => modal.key === key.toString());
		if (modalIndex === -1) return;

		const existingModal = this.modals[modalIndex];
		this.modals[modalIndex] = {
			...existingModal,
			modal: updater(existingModal.modal),
			isBackground: options?.isBackground ?? existingModal.isBackground,
		};
	}

	pop(key?: string | number): void {
		let removed: Modal | undefined;
		let wasTopmost = false;
		if (key) {
			const keyStr = key.toString();
			const idx = this.modals.findIndex((modal) => modal.key === keyStr);
			if (idx !== -1) {
				wasTopmost = idx === this.modals.length - 1;
				[removed] = this.modals.splice(idx, 1);
			}
		} else {
			removed = this.modals.pop();
			wasTopmost = true;
		}

		if (removed && wasTopmost) {
			logger.debug(`ModalStore.pop restoring focus topmost=${wasTopmost} keyboardMode=${removed.keyboardModeEnabled}`);
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

	get orderedModals(): Array<ModalWithStackInfo> {
		const topmostRegularIndex = this.modals.findLastIndex((m) => !m.isBackground);

		return this.modals.map((modal, index) => {
			const isVisible = modal.isBackground || index === topmostRegularIndex;

			const needsBackdrop = modal.isBackground || index === 0 || (index > 0 && this.modals[index - 1].isBackground);

			return {
				...modal,
				stackIndex: index,
				isVisible,
				needsBackdrop,
			};
		});
	}

	getModal(): Modal | undefined {
		return this.modals.at(-1);
	}

	hasModalOpen(): boolean {
		return this.modals.length > 0;
	}

	hasModal(key: string): boolean {
		return this.modals.some((modal) => modal.key === key);
	}

	hasModalOfType<T>(component: React.ComponentType<T>): boolean {
		return this.modals.some((modal) => modal.modal().type === component);
	}

	private getActiveElement(): HTMLElement | null {
		const active = document.activeElement;
		return active instanceof HTMLElement ? active : null;
	}

	private scheduleFocus(target: HTMLElement | null, keyboardModeEnabled: boolean): void {
		logger.debug(
			`ModalStore.scheduleFocus target=${target ? target.tagName : 'null'} keyboardMode=${keyboardModeEnabled}`,
		);
		if (!target) return;
		queueMicrotask(() => {
			if (!target.isConnected) {
				logger.debug('ModalStore.scheduleFocus aborted: target disconnected');
				return;
			}
			try {
				target.focus({preventScroll: true});
				logger.debug('ModalStore.scheduleFocus applied focus to target');
			} catch (error) {
				logger.error('ModalStore.scheduleFocus failed to focus target', error as Error);
				return;
			}
			if (keyboardModeEnabled) {
				logger.debug('ModalStore.scheduleFocus re-entering keyboard mode');
				KeyboardModeStore.enterKeyboardMode(false);
			}
		});
	}
}

export type {PushOptions};
export default new ModalStore();
