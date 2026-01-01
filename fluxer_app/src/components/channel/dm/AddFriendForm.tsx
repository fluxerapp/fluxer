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

import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as RelationshipActionCreators from '~/actions/RelationshipActionCreators';
import {APIErrorCodes} from '~/Constants';
import {Input} from '~/components/form/Input';
import {Button} from '~/components/uikit/Button/Button';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import {getApiErrorCode} from '~/utils/ApiErrorUtils';
import styles from './AddFriendForm.module.css';

interface AddFriendFormProps {
	onSuccess?: () => void;
}

export const AddFriendForm: React.FC<AddFriendFormProps> = observer(({onSuccess}) => {
	const {t} = useLingui();
	const [input, setInput] = React.useState('');
	const [isLoading, setIsLoading] = React.useState(false);
	const [resultStatus, setResultStatus] = React.useState<'success' | 'error' | null>(null);
	const [errorCode, setErrorCode] = React.useState<string | null>(null);

	const parseInput = (input: string): [string, string] => {
		const parts = input.split('#');
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
		switch (errorCode) {
			case APIErrorCodes.FRIEND_REQUEST_BLOCKED:
				return t`This user has restricted who can send them friend requests.`;
			case APIErrorCodes.CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER:
				return t`You can't send friend requests to users you've blocked. Unblock them first.`;
			case APIErrorCodes.BOTS_CANNOT_HAVE_FRIENDS:
				return t`You cannot send friend requests to bots.`;
			case APIErrorCodes.CANNOT_SEND_FRIEND_REQUEST_TO_SELF:
				return t`You cannot send a friend request to yourself.`;
			case APIErrorCodes.NO_USERS_WITH_FLUXERTAG_EXIST:
				return t`No user found with that FluxerTag.`;
			case APIErrorCodes.ALREADY_FRIENDS:
				return t`You are already friends with this user.`;
			case APIErrorCodes.DISCRIMINATOR_REQUIRED:
				return t`Please enter a valid FluxerTag (Username#0000).`;
			default:
				return t`Unable to send friend request. Please try again.`;
		}
	};

	const handleSubmit = (e: React.FormEvent) => {
		e.preventDefault();

		const [username, discriminator] = parseInput(input);

		if (!username || !discriminator || !/^\d{4}$/.test(discriminator)) {
			setResultStatus('error');
			setErrorCode(APIErrorCodes.NO_USERS_WITH_FLUXERTAG_EXIST);
			return;
		}

		setIsLoading(true);

		RelationshipActionCreators.sendFriendRequestByTag(username, discriminator)
			.then(() => {
				setIsLoading(false);
				setResultStatus('success');
				setInput('');
				onSuccess?.();
			})
			.catch((error: unknown) => {
				setIsLoading(false);
				setResultStatus('error');
				setErrorCode(getApiErrorCode(error) ?? null);
			});
	};

	const isDisabled = isLoading || !input.trim();
	const isMobile = MobileLayoutStore.isMobileLayout();

	const submitButton = (
		<Button
			type="submit"
			disabled={isDisabled}
			submitting={isLoading}
			className={isMobile ? styles.button : styles.inlineButton}
			fitContainer={false}
			compact={!isMobile}
		>
			{t`Send Request`}
		</Button>
	);

	return (
		<form onSubmit={handleSubmit} className={styles.form}>
			<div className={clsx(styles.container, !isMobile && styles.containerDesktop)}>
				<Input
					type="text"
					value={input}
					onChange={handleInputChange}
					placeholder={t`Username#0000`}
					className={clsx(
						styles.input,
						!isMobile && styles.inputDesktop,
						resultStatus === 'error' && styles.inputError,
					)}
					disabled={isLoading}
					aria-label={t`Friend's FluxerTag`}
					rightElement={!isMobile ? submitButton : undefined}
				/>
				{isMobile && submitButton}
			</div>

			{resultStatus === 'error' && <p className={styles.errorMessage}>{getErrorMessage()}</p>}
			{resultStatus === 'success' && <p className={styles.successMessage}>{t`Friend request sent!`}</p>}
		</form>
	);
});
