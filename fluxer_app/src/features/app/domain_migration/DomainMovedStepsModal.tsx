// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/app/domain_migration/DomainMovedStepsModal.module.css';
import {CLOSE_DESCRIPTOR, COPY_LINK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback} from 'react';

export type DomainMovedStepsPlatform = 'ios' | 'mac' | 'install' | 'generic';

const TITLE_DESCRIPTOR = msg({
	message: 'Get the new {productName} app',
	comment: 'Title of the modal that explains how to install the app from its new domain. productName is the app name.',
});
const INTRO_DESCRIPTOR = msg({
	message: '{productName} now lives at {host}. Install it from there, then sign in with this app.',
	comment:
		'Intro in the modal that explains how to install the app from its new domain. productName is the app name. host is the new domain, for example fluxer.com.',
});
const INSTALL_INTRO_DESCRIPTOR = msg({
	message: '{productName} now lives at {host}. Install it from there.',
	comment:
		'Intro in the modal that explains how to install the app from its new domain when the account already moved. productName is the app name. host is the new domain, for example fluxer.com.',
});
const OPEN_IN_SAFARI_STEP_DESCRIPTOR = msg({
	message: 'Open {host} in Safari.',
	comment:
		'First install step on Apple devices. host is the new domain, for example fluxer.com. Safari is the browser.',
});
const OPEN_IN_BROWSER_STEP_DESCRIPTOR = msg({
	message: 'Open {host} in your browser.',
	comment: 'First install step on other devices. host is the new domain, for example fluxer.com.',
});
const ADD_TO_HOME_SCREEN_STEP_DESCRIPTOR = msg({
	message: 'Tap Share, then Add to Home Screen.',
	comment:
		'Second install step on iPhone and iPad. Share and Add to Home Screen are the names of the Safari menu items, use the names iOS shows in this language.',
});
const ADD_TO_DOCK_STEP_DESCRIPTOR = msg({
	message: 'Choose File, then Add to Dock.',
	comment:
		'Second install step on a Mac. File and Add to Dock are the names of the Safari menu items, use the names macOS shows in this language.',
});
const INSTALL_FROM_BROWSER_STEP_DESCRIPTOR = msg({
	message: 'Install it from your browser menu.',
	comment: 'Second install step on other devices. Refers to the install option in the browser menu.',
});
const SIGN_IN_STEP_DESCRIPTOR = msg({
	message:
		'Open the new app and choose Sign in with your old {productName} app. Then choose Link a new device here and enter the code it shows.',
	comment:
		'Third install step. "Sign in with your old {productName} app" and "Link a new device" are button labels and must match their translations. productName is the app name.',
});

const OPEN_SIGNED_IN_STEP_DESCRIPTOR = msg({
	message: 'Open the new app. You are already signed in.',
	comment: 'Third install step in a desktop browser that already moved the account to the new domain.',
});

type DomainMovedSteps = readonly [MessageDescriptor, MessageDescriptor, MessageDescriptor];

const PLATFORM_STEPS: Record<DomainMovedStepsPlatform, DomainMovedSteps> = {
	ios: [OPEN_IN_SAFARI_STEP_DESCRIPTOR, ADD_TO_HOME_SCREEN_STEP_DESCRIPTOR, SIGN_IN_STEP_DESCRIPTOR],
	mac: [OPEN_IN_SAFARI_STEP_DESCRIPTOR, ADD_TO_DOCK_STEP_DESCRIPTOR, SIGN_IN_STEP_DESCRIPTOR],
	install: [OPEN_IN_BROWSER_STEP_DESCRIPTOR, INSTALL_FROM_BROWSER_STEP_DESCRIPTOR, OPEN_SIGNED_IN_STEP_DESCRIPTOR],
	generic: [OPEN_IN_BROWSER_STEP_DESCRIPTOR, INSTALL_FROM_BROWSER_STEP_DESCRIPTOR, SIGN_IN_STEP_DESCRIPTOR],
};

interface DomainMovedStepsModalProps {
	target: string;
	platform: DomainMovedStepsPlatform;
}

function DomainMovedStepsModal({target, platform}: DomainMovedStepsModalProps) {
	const {i18n} = useLingui();
	const host = new URL(target).host;
	const values = {host, productName: PRODUCT_NAME};
	const [first, second, third] = PLATFORM_STEPS[platform];
	const handleCopy = useCallback(() => {
		void TextCopyCommands.copy(i18n, `${target}/`);
	}, [i18n, target]);
	return (
		<Modal.Root size="small" centered onClose={ModalCommands.pop} data-flx="app.domain-moved-steps-modal.modal-root">
			<Modal.Header title={i18n._(TITLE_DESCRIPTOR, values)} data-flx="app.domain-moved-steps-modal.modal-header" />
			<Modal.Content data-flx="app.domain-moved-steps-modal.modal-content">
				<Modal.ContentLayout className={styles.content} data-flx="app.domain-moved-steps-modal.content">
					<Modal.Description data-flx="app.domain-moved-steps-modal.description">
						{i18n._(platform === 'install' ? INSTALL_INTRO_DESCRIPTOR : INTRO_DESCRIPTOR, values)}
					</Modal.Description>
					<ol className={styles.steps} data-flx="app.domain-moved-steps-modal.steps">
						<li className={styles.step} data-flx="app.domain-moved-steps-modal.step-open">
							{i18n._(first, values)}
						</li>
						<li className={styles.step} data-flx="app.domain-moved-steps-modal.step-install">
							{i18n._(second, values)}
						</li>
						<li className={styles.step} data-flx="app.domain-moved-steps-modal.step-sign-in">
							{i18n._(third, values)}
						</li>
					</ol>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="app.domain-moved-steps-modal.modal-footer">
				<Button variant="secondary" onClick={handleCopy} data-flx="app.domain-moved-steps-modal.button.copy-link">
					{i18n._(COPY_LINK_DESCRIPTOR)}
				</Button>
				<Button variant="primary" onClick={ModalCommands.pop} data-flx="app.domain-moved-steps-modal.button.close">
					{i18n._(CLOSE_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
}

export function showDomainMovedStepsModal(target: string, platform: DomainMovedStepsPlatform): void {
	ModalCommands.push(
		modal(() => (
			<DomainMovedStepsModal
				target={target}
				platform={platform}
				data-flx="app.domain-moved-steps-modal.show-domain-moved-steps-modal.domain-moved-steps-modal"
			/>
		)),
	);
}
