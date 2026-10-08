// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {closeBottomSheetThen} from '@app/features/ui/bottom_sheet/BottomSheetTransitionUtils';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {getActivePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import Modal from '@app/features/ui/state/Modal';
import type {ModalRender, ModalType} from '@app/features/ui/state/ModalRender';
import type React from 'react';

const logger = new Logger('Modal');

let modalUniqueIdCounter = 0;

const generateModalKey = (): string => `modal${++modalUniqueIdCounter}`;
const BACKGROUND_MODAL_TYPES: ReadonlySet<ModalType> = new Set(['user-settings', 'guild-settings', 'channel-settings']);
const isBackgroundModal = (modalType: ModalType | undefined): boolean => {
	return modalType != null && BACKGROUND_MODAL_TYPES.has(modalType);
};
const isDuplicateModal = (modalType: ModalType | undefined): boolean => {
	if (modalType == null || !Modal.hasModalOfModalType(modalType)) return false;
	logger.debug(`Skipping duplicate modal of type: ${modalType}`);
	return true;
};
const getCommandOwnerDocument = (): Document => getActivePortalHost()?.ownerDocument ?? document;
const getPushOptions = (isBackground: boolean) => {
	const activePortalHost = getActivePortalHost();
	const isPoppedOut = activePortalHost?.ownerDocument !== document;
	const forceMainWindow = isBackground && isPoppedOut;
	return {
		isBackground,
		forceMainWindow,
		portalHost: forceMainWindow ? null : activePortalHost,
	};
};

interface PushToPortalHostOptions {
	readonly modal: ModalRender;
	readonly portalHost: HTMLElement;
}

interface ExplicitPortalHostPush {
	readonly portalHost: HTMLElement;
}

function pushModal(modal: ModalRender, explicitPortalHost?: ExplicitPortalHostPush): void {
	ContextMenuCommands.close();
	if (isDuplicateModal(modal.modalType)) return;
	const isBackground = isBackgroundModal(modal.modalType);
	const key = generateModalKey();
	logger.debug(`Pushing modal: ${key} (background=${isBackground})`);
	const options =
		explicitPortalHost == null
			? getPushOptions(isBackground)
			: {isBackground, portalHost: explicitPortalHost.portalHost};
	Modal.push(modal, key, options);
}

export function modal(render: () => React.ReactElement, modalType?: ModalType): ModalRender {
	return Object.assign(render, {modalType});
}

export function push(modal: ModalRender): void {
	pushModal(modal);
}

export function pushToPortalHost({modal, portalHost}: PushToPortalHostOptions): void {
	pushModal(modal, {portalHost});
}

export function pushWithKey(modal: ModalRender, key: string): void {
	ContextMenuCommands.close();
	if (isDuplicateModal(modal.modalType)) return;
	const isBackground = isBackgroundModal(modal.modalType);
	if (Modal.hasModal(key)) {
		logger.debug(`Updating existing modal with key: ${key}`);
		Modal.update(key, () => modal, isBackground ? getPushOptions(true) : {isBackground});
		return;
	}
	logger.debug(`Pushing modal with key: ${key} (background=${isBackground})`);
	Modal.push(modal, key, getPushOptions(isBackground));
}

export function pushAfterBottomSheetClose(onClose: () => void, modal: ModalRender): void {
	closeBottomSheetThen(onClose, () => push(modal));
}

export function pushWithKeyAfterBottomSheetClose(onClose: () => void, modal: ModalRender, key: string): void {
	closeBottomSheetThen(onClose, () => pushWithKey(modal, key));
}

export function runAfterBottomSheetClose(onClose: () => void, action: () => void): void {
	closeBottomSheetThen(onClose, action);
}

export function update(key: string, updater: (currentModal: ModalRender) => ModalRender): void {
	logger.debug(`Updating modal with key: ${key}`);
	Modal.update(key, updater);
}

export function pop(): void {
	logger.debug('Popping most recent modal');
	Modal.pop(undefined, getCommandOwnerDocument());
}

export function getTopModalKey(): string | null {
	return Modal.getModal(getCommandOwnerDocument())?.key ?? null;
}

export function popWithKey(key: string): void {
	if (!Modal.hasModal(key)) return;
	logger.debug(`Popping modal with key: ${key}`);
	Modal.pop(key);
}

export function popByType<T>(component: React.ComponentType<T>): void {
	logger.debug(`Popping modal by type: ${component.displayName ?? component.name ?? 'unknown'}`);
	Modal.popByType(component, getCommandOwnerDocument());
}

export function popByModalType(modalType: ModalType): void {
	logger.debug(`Popping modal by modal type: ${modalType}`);
	Modal.popByModalType(modalType, getCommandOwnerDocument());
}

export function popAllByType<T>(component: React.ComponentType<T>): void {
	logger.debug(`Popping all modals by type: ${component.displayName ?? component.name ?? 'unknown'}`);
	const ownerDocument = getCommandOwnerDocument();
	while (Modal.hasModalOfType(component, ownerDocument)) {
		Modal.popByType(component, ownerDocument);
	}
}

export function popAll(): void {
	logger.debug('Popping all modals');
	Modal.popAll();
}
