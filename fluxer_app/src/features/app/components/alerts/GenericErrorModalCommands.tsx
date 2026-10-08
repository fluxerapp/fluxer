// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {ReactNode} from 'react';

interface GenericErrorModalContentOptions {
	title: string | (() => string);
	message: ReactNode | (() => ReactNode);
	dataFlx?: string;
}

interface GenericErrorModalOptions extends GenericErrorModalContentOptions {
	defer?: boolean;
}

interface GenericErrorModalPortalOptions extends GenericErrorModalContentOptions {
	portalHost: HTMLElement;
}

function createGenericErrorModal({title, message, dataFlx}: GenericErrorModalContentOptions) {
	return modal(() => (
		<GenericErrorModal
			title={typeof title === 'function' ? title() : title}
			message={typeof message === 'function' ? message() : message}
			data-flx={dataFlx}
		/>
	));
}

export function showGenericErrorModal({defer, ...content}: GenericErrorModalOptions): void {
	const pushErrorModal = () => ModalCommands.push(createGenericErrorModal(content));
	if (defer) {
		window.setTimeout(pushErrorModal, 0);
		return;
	}
	pushErrorModal();
}

export function showGenericErrorModalInPortal({portalHost, ...content}: GenericErrorModalPortalOptions): void {
	if (!portalHost.isConnected) return;
	ModalCommands.pushToPortalHost({modal: createGenericErrorModal(content), portalHost});
}
