// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {SUPPORT_EMAIL} from '@app/features/app/config/I18nDisplayConstants';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import Users from '@app/features/user/state/Users';
import {
	buildMessagingPausedHelpEmail,
	TEXT_DID_NOT_GO_THROUGH_DESCRIPTOR,
} from '@app/features/user/utils/MessagingPausedHelpEmail';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const MESSAGING_PAUSED_HELP_BODY_DESCRIPTOR = msg({
	message: 'Email {supportEmail} and a person will check your account. You can also reply to the email we sent you.',
	comment:
		'Body of the help dialog opened from "My text didn\'t go through" while messaging is paused on the account. supportEmail is the support email address. Keep it short, calm and neutral.',
});
const COUNTRY_YOU_ARE_IN_DESCRIPTOR = msg({
	message: "Country you're in",
	comment:
		'Field label in the help dialog opened from "My text didn\'t go through". The user types the country they are in so support can help.',
});
const COPY_DETAILS_DESCRIPTOR = msg({
	message: 'Copy details',
	comment:
		'Button in the help dialog opened from "My text didn\'t go through". Copies the account details for a support email to the clipboard.',
});
const EMAIL_SUPPORT_DESCRIPTOR = msg({
	message: 'Email support',
	comment:
		'Primary button in the help dialog opened from "My text didn\'t go through". Opens a new email to support with the account details filled in.',
});

const MessagingPausedHelpModal = observer(() => {
	const {i18n} = useLingui();
	const [country, setCountry] = useState('');
	const user = Users.currentUser;
	if (!user) return null;
	const email = buildMessagingPausedHelpEmail(i18n, user, country);
	return (
		<Modal.Root size="small" centered data-flx="user.messaging-paused-help-modal.modal-root">
			<Modal.Header
				title={i18n._(TEXT_DID_NOT_GO_THROUGH_DESCRIPTOR)}
				data-flx="user.messaging-paused-help-modal.modal-header"
			/>
			<Modal.Content data-flx="user.messaging-paused-help-modal.modal-content">
				<Modal.ContentLayout data-flx="user.messaging-paused-help-modal.modal-content-layout">
					<Modal.Description data-flx="user.messaging-paused-help-modal.modal-description">
						{i18n._(MESSAGING_PAUSED_HELP_BODY_DESCRIPTOR, {supportEmail: SUPPORT_EMAIL})}
					</Modal.Description>
					<Modal.InputGroup data-flx="user.messaging-paused-help-modal.modal-input-group">
						<Input
							label={i18n._(COUNTRY_YOU_ARE_IN_DESCRIPTOR)}
							value={country}
							onChange={(event: React.ChangeEvent<HTMLInputElement>) => setCountry(event.target.value)}
							autoComplete="country-name"
							maxLength={100}
							data-flx="user.messaging-paused-help-modal.input.country"
						/>
					</Modal.InputGroup>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="user.messaging-paused-help-modal.footer">
				<Button
					variant="secondary"
					onClick={() => void TextCopyCommands.copy(i18n, `${SUPPORT_EMAIL}\n\n${email.body}`)}
					data-flx="user.messaging-paused-help-modal.button.copy-details"
				>
					{i18n._(COPY_DETAILS_DESCRIPTOR)}
				</Button>
				<Button
					onClick={() => void openExternalUrl(email.mailto)}
					data-flx="user.messaging-paused-help-modal.button.email-support"
				>
					{i18n._(EMAIL_SUPPORT_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});

export function openMessagingPausedHelpModal(): void {
	ModalCommands.push(
		modal(() => (
			<MessagingPausedHelpModal data-flx="user.messaging-paused-help-modal.open.messaging-paused-help-modal" />
		)),
	);
}
