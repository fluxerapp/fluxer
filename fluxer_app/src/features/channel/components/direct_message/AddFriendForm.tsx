// SPDX-License-Identifier: AGPL-3.0-or-later

import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {EXAMPLE_FLUXER_TAG_FULL} from '@app/features/app/config/I18nDisplayConstants';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import styles from '@app/features/channel/components/direct_message/AddFriendForm.module.css';
import {CLAIM_ACCOUNT_DESCRIPTOR, VERIFY_EMAIL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as RelationshipCommands from '@app/features/relationship/commands/RelationshipCommands';
import {getSendFriendRequestErrorMessage} from '@app/features/relationship/utils/RelationshipActionUtils';
import {OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {EnvelopeSimpleIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useState} from 'react';

const NO_USER_FOUND_WITH_THAT_USERNAME_DESCRIPTOR = msg({
	message: 'No user found with that username.',
	comment: 'Empty-state text in the channel and chat add friend form.',
});
const PLEASE_ENTER_A_VALID_USERNAME_DESCRIPTOR = msg({
	message: 'Enter a valid username ({exampleFluxerTagFull}).',
	comment:
		'Description text in the channel and chat add friend form. Preserve {exampleFluxerTagFull}; it is inserted by code.',
});
const SEND_REQUEST_DESCRIPTOR = msg({
	message: 'Send request',
	comment: 'Button or menu action label in the channel and chat add friend form. Keep it concise.',
});
const FRIEND_S_USERNAME_DESCRIPTOR = msg({
	message: "Friend's username",
	comment: 'Short label in the channel and chat add friend form. Keep it concise.',
});

interface AddFriendFormProps {
	onSuccess?: () => void;
}

export const AddFriendForm: React.FC<AddFriendFormProps> = observer(({onSuccess}) => {
	const {i18n} = useLingui();
	const [input, setInput] = useState('');
	const [isLoading, setIsLoading] = useState(false);
	const [resultStatus, setResultStatus] = useState<'success' | 'error' | null>(null);
	const [errorCode, setErrorCode] = useState<string | null>(null);
	const currentUser = Users.currentUser;
	const isClaimed = currentUser?.isClaimed() ?? true;
	if (!isClaimed) {
		return (
			<StatusSlate
				Icon={WarningCircleIcon}
				title={<Trans>Claim your account</Trans>}
				description={<Trans>Claim your account to send friend requests.</Trans>}
				actions={[
					{
						text: i18n._(CLAIM_ACCOUNT_DESCRIPTOR),
						onClick: () => openClaimAccountModal({force: true}),
						variant: 'primary',
					},
				]}
				data-flx="channel.direct-message.add-friend-form.status-slate"
			/>
		);
	}
	if (currentUser?.verified === false) {
		return (
			<StatusSlate
				Icon={EnvelopeSimpleIcon}
				title={<Trans>Verify your email</Trans>}
				description={<Trans>You need to verify your email address before you can send friend requests.</Trans>}
				actions={[
					{
						text: i18n._(VERIFY_EMAIL_DESCRIPTOR),
						onClick: () =>
							ModalCommands.push(
								modal(() => (
									<UserSettingsModal
										initialTab="account_security"
										data-flx="channel.direct-message.add-friend-form.on-click.user-settings-modal"
									/>
								)),
							),
						variant: 'primary',
					},
				]}
				data-flx="channel.direct-message.add-friend-form.status-slate--2"
			/>
		);
	}
	const parseInput = (input: string): [string, string] => {
		const parts = input['split']('#');
		if (parts.length > 1) {
			return [parts[0], parts.slice(1).join('#')];
		}
		return [input, '0000'];
	};
	const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
		setInput(e.target.value);
		if (resultStatus) {
			setResultStatus(null);
			setErrorCode(null);
		}
	};
	const getErrorMessage = () => {
		if (!errorCode) {
			return getSendFriendRequestErrorMessage(i18n, null, null);
		}
		if (errorCode === APIErrorCodes.NO_USERS_WITH_FLUXERTAG_EXIST) {
			return i18n._(NO_USER_FOUND_WITH_THAT_USERNAME_DESCRIPTOR);
		}
		if (errorCode === APIErrorCodes.DISCRIMINATOR_REQUIRED) {
			return i18n._(PLEASE_ENTER_A_VALID_USERNAME_DESCRIPTOR, {exampleFluxerTagFull: EXAMPLE_FLUXER_TAG_FULL});
		}
		return getSendFriendRequestErrorMessage(i18n, errorCode, null);
	};
	const handleSubmit = (e: React.FormEvent) => {
		e.preventDefault();
		const [username, discriminator] = parseInput(input['trim']());
		if (!username || !discriminator || !/^\d{4}$/.test(discriminator)) {
			setResultStatus('error');
			setErrorCode(APIErrorCodes.NO_USERS_WITH_FLUXERTAG_EXIST);
			return;
		}
		setIsLoading(true);
		RelationshipCommands.sendFriendRequestByTag(username, discriminator)
			.then(() => {
				setIsLoading(false);
				setResultStatus('success');
				setInput('');
				onSuccess?.();
			})
			.catch((error: unknown) => {
				setIsLoading(false);
				setResultStatus('error');
				setErrorCode(failureCode(error) ?? null);
			});
	};
	const isDisabled = isLoading || !input['trim']();
	const isMobile = MobileLayout.isMobileLayout();
	const submitButton = (
		<Button
			type="submit"
			disabled={isDisabled}
			submitting={isLoading}
			className={isMobile ? styles.button : styles.inlineButton}
			compact={!isMobile}
			data-flx="channel.direct-message.add-friend-form.button.submit"
		>
			{i18n._(SEND_REQUEST_DESCRIPTOR)}
		</Button>
	);
	return (
		<form onSubmit={handleSubmit} className={styles.form} data-flx="channel.direct-message.add-friend-form.form.submit">
			<div
				className={clsx(styles.container, !isMobile && styles.containerDesktop)}
				data-flx="channel.direct-message.add-friend-form.container"
			>
				<Input
					type="text"
					value={input}
					onChange={handleInputChange}
					placeholder={EXAMPLE_FLUXER_TAG_FULL}
					className={clsx(
						styles.input,
						!isMobile && styles.inputDesktop,
						resultStatus === 'error' && styles.inputError,
					)}
					disabled={isLoading}
					aria-label={i18n._(FRIEND_S_USERNAME_DESCRIPTOR)}
					rightElement={!isMobile ? submitButton : undefined}
					data-flx="channel.direct-message.add-friend-form.input.text"
				/>
				{isMobile && submitButton}
			</div>
			{resultStatus === 'error' && (
				<p className={styles.errorMessage} data-flx="channel.direct-message.add-friend-form.error-message">
					{getErrorMessage()}
				</p>
			)}
			{resultStatus === 'success' && (
				<p className={styles.successMessage} data-flx="channel.direct-message.add-friend-form.success-message">
					{i18n._(OUTGOING_FRIEND_REQUEST_STATUS_DESCRIPTOR)}
				</p>
			)}
		</form>
	);
});
