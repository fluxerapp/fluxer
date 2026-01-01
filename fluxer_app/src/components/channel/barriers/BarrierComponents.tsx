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

import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ClaimAccountModal} from '~/components/modals/ClaimAccountModal';
import {PhoneAddModal} from '~/components/modals/PhoneAddModal';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {Button} from '~/components/uikit/Button/Button';
import {unblockUser} from '~/utils/RelationshipActionUtils';
import wrapperStyles from '../textarea/InputWrapper.module.css';
import styles from './BarrierComponents.module.css';

interface BarrierProps {
	onAction?: () => void;
}

interface TimedBarrierProps extends BarrierProps {
	initialTimeRemaining?: number;
}

const BarrierBase = observer(({message, action}: {message: React.ReactNode; action?: React.ReactNode}) => {
	return (
		<div
			className={clsx(
				wrapperStyles.box,
				wrapperStyles.wrapperSides,
				wrapperStyles.roundedAll,
				wrapperStyles.bottomSpacing,
			)}
			style={{minHeight: 'var(--input-container-min-height)'}}
		>
			<div
				className={wrapperStyles.barInner}
				style={{gridTemplateColumns: '1fr auto', minHeight: 'var(--input-container-min-height)'}}
			>
				<div className={styles.message}>{message}</div>
				{action && <div>{action}</div>}
			</div>
		</div>
	);
});

const CountdownTimer = observer(({initialTime}: {initialTime: number}) => {
	const [timeRemaining, setTimeRemaining] = useState<number>(initialTime);

	useEffect(() => {
		if (timeRemaining <= 0) return;

		const interval = setInterval(() => {
			setTimeRemaining((prev) => {
				if (prev <= 1000) {
					clearInterval(interval);
					return 0;
				}
				return prev - 1000;
			});
		}, 1000);

		return () => clearInterval(interval);
	}, []);

	const formatTime = (ms: number): string => {
		const totalSeconds = Math.ceil(ms / 1000);
		const minutes = Math.floor(totalSeconds / 60);
		const seconds = totalSeconds % 60;
		return `${minutes}:${seconds.toString().padStart(2, '0')}`;
	};

	if (timeRemaining <= 0) {
		return null;
	}

	return <div className={styles.timer}>{formatTime(timeRemaining)}</div>;
});

export const UnclaimedAccountBarrier = observer(({onAction}: BarrierProps) => {
	return (
		<BarrierBase
			message={<Trans>You need to claim your account to send messages in this community.</Trans>}
			action={
				<Button
					small={true}
					onClick={() => {
						onAction?.();
						ModalActionCreators.push(modal(() => <ClaimAccountModal />));
					}}
				>
					<Trans>Claim Account</Trans>
				</Button>
			}
		/>
	);
});

export const UnverifiedEmailBarrier = observer(({onAction}: BarrierProps) => {
	return (
		<BarrierBase
			message={<Trans>You need to verify your email to send messages in this community.</Trans>}
			action={
				<Button
					small={true}
					onClick={() => {
						onAction?.();
						ModalActionCreators.push(modal(() => <UserSettingsModal initialTab="account_security" />));
					}}
				>
					<Trans>Verify Email</Trans>
				</Button>
			}
		/>
	);
});

export const AccountTooNewBarrier = observer(({initialTimeRemaining = 5 * 60 * 1000}: TimedBarrierProps) => {
	return (
		<BarrierBase
			message={<Trans>Your account is too new to send messages in this community.</Trans>}
			action={initialTimeRemaining > 0 ? <CountdownTimer initialTime={initialTimeRemaining} /> : null}
		/>
	);
});

export const NotMemberLongEnoughBarrier = observer(({initialTimeRemaining = 10 * 60 * 1000}: TimedBarrierProps) => {
	return (
		<BarrierBase
			message={<Trans>You haven't been a member of this community long enough to send messages.</Trans>}
			action={initialTimeRemaining > 0 ? <CountdownTimer initialTime={initialTimeRemaining} /> : null}
		/>
	);
});

export const NoPhoneNumberBarrier = observer(({onAction}: BarrierProps) => {
	return (
		<BarrierBase
			message={<Trans>You need to add a phone number to send messages in this community.</Trans>}
			action={
				<Button
					small={true}
					onClick={() => {
						onAction?.();
						ModalActionCreators.push(modal(() => <PhoneAddModal />));
					}}
				>
					<Trans>Add Phone</Trans>
				</Button>
			}
		/>
	);
});

export const SendMessageDisabledBarrier = observer(() => {
	return (
		<BarrierBase
			message={
				<Trans>
					Messaging has been temporarily disabled in this community by platform staff. This is usually due to potential
					spam or abuse detection.
				</Trans>
			}
			action={null}
		/>
	);
});

export const TimeoutBarrier = observer(({initialTimeRemaining = 0}: TimedBarrierProps) => {
	return (
		<BarrierBase
			message={
				<Trans>
					You're currently timed out from this community. Messaging, reactions, and voice access are blocked until the
					timeout expires.
				</Trans>
			}
			action={initialTimeRemaining > 0 ? <CountdownTimer initialTime={initialTimeRemaining} /> : null}
		/>
	);
});

export const DefaultBarrier = observer(() => {
	return <BarrierBase message={<Trans>You cannot send messages in this community.</Trans>} action={null} />;
});

interface BlockedUserBarrierProps extends BarrierProps {
	userId: string;
	username: string;
}

export const BlockedUserBarrier = observer(({userId, username, onAction}: BlockedUserBarrierProps) => {
	const {i18n} = useLingui();
	const handleUnblock = async () => {
		await unblockUser(i18n, userId);
		onAction?.();
	};

	return (
		<BarrierBase
			message={<Trans>You have blocked {username}. Unblock them to send messages.</Trans>}
			action={
				<Button small={true} onClick={handleUnblock}>
					<Trans>Unblock</Trans>
				</Button>
			}
		/>
	);
});

export const UnclaimedDMBarrier = observer(({onAction}: BarrierProps) => {
	return (
		<BarrierBase
			message={<Trans>You need to claim your account to send direct messages.</Trans>}
			action={
				<Button
					small={true}
					onClick={() => {
						onAction?.();
						ModalActionCreators.push(modal(() => <ClaimAccountModal />));
					}}
				>
					<Trans>Claim Account</Trans>
				</Button>
			}
		/>
	);
});
