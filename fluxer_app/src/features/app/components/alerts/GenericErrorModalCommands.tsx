// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {ReactNode} from 'react';

type GenericErrorModalContentOptions = {
	title?: string | (() => string);
	dataFlx?: string;
} & (
	| {message: ReactNode | (() => ReactNode); getContent?: never}
	| {message?: never; getContent: () => {title: string; message: ReactNode}}
);

type GenericErrorModalOptions = GenericErrorModalContentOptions & {defer?: boolean};
type GenericErrorModalPortalOptions = GenericErrorModalContentOptions & {portalHost: HTMLElement};

function createGenericErrorModal({
	title = () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
	message,
	dataFlx,
	getContent,
}: GenericErrorModalContentOptions) {
	return modal(() => {
		const content = getContent?.();
		return (
			<GenericErrorModal
				title={content?.title ?? (typeof title === 'function' ? title() : title)}
				message={content?.message ?? (typeof message === 'function' ? message() : message)}
				data-flx={dataFlx}
			/>
		);
	});
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
