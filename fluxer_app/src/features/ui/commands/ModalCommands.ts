// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelSettingsModal} from '@app/features/channel/components/modals/ChannelSettingsModal';
import {GuildSettingsModal} from '@app/features/guild/components/modals/GuildSettingsModal';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {closeBottomSheetThen} from '@app/features/ui/bottom_sheet/BottomSheetTransitionUtils';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {getActivePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import Modal from '@app/features/ui/state/Modal';
import type {ModalRender} from '@app/features/ui/state/ModalRender';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import type React from 'react';

const logger = new Logger('Modal');

let modalUniqueIdCounter = 0;

const generateModalKey = (): string => `modal${++modalUniqueIdCounter}`;
const BACKGROUND_MODAL_TYPES = [UserSettingsModal, GuildSettingsModal, ChannelSettingsModal] as const;
const isBackgroundModal = (element: React.ReactElement): boolean => {
	return BACKGROUND_MODAL_TYPES.some((type) => element.type === type);
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

export function modal(render: () => React.ReactElement): ModalRender {
	return render as ModalRender;
}

export function push(modal: ModalRender): void {
	ContextMenuCommands.close();
	const renderedModal = modal();
	const isBackground = isBackgroundModal(renderedModal);
	if (renderedModal.type === UserSettingsModal && Modal.hasModalOfType(UserSettingsModal)) {
		logger.debug('Skipping duplicate UserSettingsModal');
		return;
	}
	if (renderedModal.type === GuildSettingsModal && Modal.hasModalOfType(GuildSettingsModal)) {
		logger.debug('Skipping duplicate GuildSettingsModal');
		return;
	}
	if (renderedModal.type === ChannelSettingsModal && Modal.hasModalOfType(ChannelSettingsModal)) {
		logger.debug('Skipping duplicate ChannelSettingsModal');
		return;
	}
	const key = generateModalKey();
	logger.debug(`Pushing modal: ${key} (background=${isBackground})`);
	Modal.push(modal, key, getPushOptions(isBackground));
}

export function pushWithKey(modal: ModalRender, key: string): void {
	ContextMenuCommands.close();
	const renderedModal = modal();
	const isBackground = isBackgroundModal(renderedModal);
	if (renderedModal.type === UserSettingsModal && Modal.hasModalOfType(UserSettingsModal)) {
		logger.debug('Skipping duplicate UserSettingsModal');
		return;
	}
	if (renderedModal.type === GuildSettingsModal && Modal.hasModalOfType(GuildSettingsModal)) {
		logger.debug('Skipping duplicate GuildSettingsModal');
		return;
	}
	if (renderedModal.type === ChannelSettingsModal && Modal.hasModalOfType(ChannelSettingsModal)) {
		logger.debug('Skipping duplicate ChannelSettingsModal');
		return;
	}
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
