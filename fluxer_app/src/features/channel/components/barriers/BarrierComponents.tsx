// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import styles from '@app/features/channel/components/barriers/BarrierComponents.module.css';
import wrapperStyles from '@app/features/channel/components/textarea/InputWrapper.module.css';
import textareaStyles from '@app/features/channel/components/textarea/TextareaInput.module.css';
import {CLAIM_ACCOUNT_DESCRIPTOR, VERIFY_EMAIL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {unblockUser} from '@app/features/relationship/utils/RelationshipActionUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {PhoneAddModal} from '@app/features/user/components/modals/PhoneAddModal';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	ClockIcon,
	EnvelopeSimpleIcon,
	InfoIcon,
	PhoneIcon,
	ProhibitIcon,
	ShieldWarningIcon,
	TimerIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

interface BarrierProps {
	onAction?: () => void;
}

interface TimedBarrierProps extends BarrierProps {
	initialTimeRemaining?: number;
}

interface BarrierBaseProps {
	message: React.ReactNode;
	action?: React.ReactNode;
	icon: React.ReactElement;
}

const SYSTEM_ANNOUNCEMENTS_FROM_STAFF_DESCRIPTOR = msg({
	message: "System announcements from {productName} staff. You can't reply here.",
	comment: 'Read-only system DM barrier message. productName is the Fluxer product name.',
});
const BarrierBase = observer(({message, action, icon}: BarrierBaseProps) => {
	const hasAction = Boolean(action);
	return (
		<div
			className={clsx(
				wrapperStyles.box,
				wrapperStyles.wrapperSides,
				textareaStyles.textareaOuter,
				textareaStyles.textareaOuterRow,
				wrapperStyles.roundedAll,
			)}
			data-flx="channel.barriers.barrier-components.barrier-base.div"
		>
			<div
				className={clsx(styles.barrierLayout, !hasAction && styles.barrierLayoutNoAction)}
				data-flx="channel.barriers.barrier-components.barrier-base.barrier-layout"
			>
				<div
					className={clsx(textareaStyles.uploadButtonColumn, textareaStyles.sideButtonPadding)}
					data-flx="channel.barriers.barrier-components.barrier-base.div--2"
				>
					<div
						aria-hidden={true}
						className={styles.icon}
						data-flx="channel.barriers.barrier-components.barrier-base.icon"
					>
						{icon}
					</div>
				</div>
				<div
					className={clsx(textareaStyles.contentAreaDense, styles.messageArea)}
					data-flx="channel.barriers.barrier-components.barrier-base.message-area"
				>
					<div className={styles.message} data-flx="channel.barriers.barrier-components.barrier-base.message">
						{message}
					</div>
				</div>
				{hasAction && (
					<div
						className={clsx(textareaStyles.buttonContainerDense, textareaStyles.sideButtonPadding, styles.actionArea)}
						data-flx="channel.barriers.barrier-components.barrier-base.action-area"
					>
						{action}
					</div>
				)}
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
	return (
		<div className={styles.timer} data-flx="channel.barriers.barrier-components.countdown-timer.timer">
			{formatTime(timeRemaining)}
		</div>
	);
});
export const UnclaimedAccountBarrier = observer(({onAction}: BarrierProps) => {
	const {i18n} = useLingui();
	return (
		<BarrierBase
			message={<Trans>You need to claim your account to send messages in this community.</Trans>}
			icon={
				<ShieldWarningIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.unclaimed-account-barrier.shield-warning-icon"
				/>
			}
			action={
				<Button
					small={true}
					onClick={() => {
						onAction?.();
						openClaimAccountModal({force: true});
					}}
					data-flx="channel.barriers.barrier-components.unclaimed-account-barrier.button.action"
				>
					{i18n._(CLAIM_ACCOUNT_DESCRIPTOR)}
				</Button>
			}
			data-flx="channel.barriers.barrier-components.unclaimed-account-barrier.barrier-base"
		/>
	);
});
export const UnverifiedEmailBarrier = observer(({onAction}: BarrierProps) => {
	const {i18n} = useLingui();
	return (
		<BarrierBase
			message={<Trans>You need to verify your email to send messages in this community.</Trans>}
			icon={
				<EnvelopeSimpleIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.unverified-email-barrier.envelope-simple-icon"
				/>
			}
			action={
				<Button
					small={true}
					onClick={() => {
						onAction?.();
						ModalCommands.push(
							modal(() => (
								<UserSettingsModal
									initialTab="account_security"
									data-flx="channel.barriers.barrier-components.unverified-email-barrier.user-settings-modal"
								/>
							)),
						);
					}}
					data-flx="channel.barriers.barrier-components.unverified-email-barrier.button.action"
				>
					{i18n._(VERIFY_EMAIL_DESCRIPTOR)}
				</Button>
			}
			data-flx="channel.barriers.barrier-components.unverified-email-barrier.barrier-base"
		/>
	);
});
export const AccountTooNewBarrier = observer(({initialTimeRemaining = 5 * 60 * 1000}: TimedBarrierProps) => {
	return (
		<BarrierBase
			message={<Trans>Your account is too new to send messages in this community.</Trans>}
			icon={
				<ClockIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.account-too-new-barrier.clock-icon"
				/>
			}
			action={
				initialTimeRemaining > 0 ? (
					<CountdownTimer
						initialTime={initialTimeRemaining}
						data-flx="channel.barriers.barrier-components.account-too-new-barrier.countdown-timer"
					/>
				) : null
			}
			data-flx="channel.barriers.barrier-components.account-too-new-barrier.barrier-base"
		/>
	);
});
export const NotMemberLongEnoughBarrier = observer(({initialTimeRemaining = 10 * 60 * 1000}: TimedBarrierProps) => {
	return (
		<BarrierBase
			message={<Trans>You haven't been a member of this community long enough to send messages.</Trans>}
			icon={
				<ClockIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.not-member-long-enough-barrier.clock-icon"
				/>
			}
			action={
				initialTimeRemaining > 0 ? (
					<CountdownTimer
						initialTime={initialTimeRemaining}
						data-flx="channel.barriers.barrier-components.not-member-long-enough-barrier.countdown-timer"
					/>
				) : null
			}
			data-flx="channel.barriers.barrier-components.not-member-long-enough-barrier.barrier-base"
		/>
	);
});
export const NoPhoneNumberBarrier = observer(({onAction}: BarrierProps) => {
	return (
		<BarrierBase
			message={<Trans>You need to verify a phone number to send messages in this community.</Trans>}
			icon={
				<PhoneIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.no-phone-number-barrier.phone-icon"
				/>
			}
			action={
				<Button
					small={true}
					onClick={() => {
						onAction?.();
						ModalCommands.push(
							modal(() => (
								<PhoneAddModal data-flx="channel.barriers.barrier-components.no-phone-number-barrier.phone-add-modal" />
							)),
						);
					}}
					data-flx="channel.barriers.barrier-components.no-phone-number-barrier.button.action"
				>
					<Trans>Verify phone</Trans>
				</Button>
			}
			data-flx="channel.barriers.barrier-components.no-phone-number-barrier.barrier-base"
		/>
	);
});
export const SendMessageDisabledBarrier = observer(() => {
	return (
		<BarrierBase
			message={<Trans>Messaging is temporarily paused in this community.</Trans>}
			icon={
				<WarningCircleIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.send-message-disabled-barrier.warning-circle-icon"
				/>
			}
			action={null}
			data-flx="channel.barriers.barrier-components.send-message-disabled-barrier.barrier-base"
		/>
	);
});
export const TimeoutBarrier = observer(({initialTimeRemaining = 0}: TimedBarrierProps) => {
	return (
		<BarrierBase
			message={<Trans>You're timed out. Messaging, reactions, and voice are paused until the timeout expires.</Trans>}
			icon={
				<TimerIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.timeout-barrier.timer-icon"
				/>
			}
			action={
				initialTimeRemaining > 0 ? (
					<CountdownTimer
						initialTime={initialTimeRemaining}
						data-flx="channel.barriers.barrier-components.timeout-barrier.countdown-timer"
					/>
				) : null
			}
			data-flx="channel.barriers.barrier-components.timeout-barrier.barrier-base"
		/>
	);
});
export const DefaultBarrier = observer(() => {
	return (
		<BarrierBase
			message={<Trans>You can't send messages in this community.</Trans>}
			icon={
				<InfoIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.default-barrier.info-icon"
				/>
			}
			action={null}
			data-flx="channel.barriers.barrier-components.default-barrier.barrier-base"
		/>
	);
});
export const SystemDmBarrier = observer(() => {
	const {i18n} = useLingui();
	return (
		<BarrierBase
			message={i18n._(SYSTEM_ANNOUNCEMENTS_FROM_STAFF_DESCRIPTOR, {productName: PRODUCT_NAME})}
			icon={
				<InfoIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.system-dm-barrier.info-icon"
				/>
			}
			action={null}
			data-flx="channel.barriers.barrier-components.system-dm-barrier.barrier-base"
		/>
	);
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
			icon={
				<ProhibitIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.blocked-user-barrier.prohibit-icon"
				/>
			}
			action={
				<Button
					small={true}
					onClick={handleUnblock}
					data-flx="channel.barriers.barrier-components.blocked-user-barrier.button.unblock"
				>
					<Trans>Unblock</Trans>
				</Button>
			}
			data-flx="channel.barriers.barrier-components.blocked-user-barrier.barrier-base"
		/>
	);
});
export const UnclaimedDMBarrier = observer(({onAction}: BarrierProps) => {
	const {i18n} = useLingui();
	return (
		<BarrierBase
			message={<Trans>You need to claim your account to send direct messages.</Trans>}
			icon={
				<ShieldWarningIcon
					size={remFromPx(18)}
					weight="fill"
					data-flx="channel.barriers.barrier-components.unclaimed-dm-barrier.shield-warning-icon"
				/>
			}
			action={
				<Button
					small={true}
					onClick={() => {
						onAction?.();
						openClaimAccountModal({force: true});
					}}
					data-flx="channel.barriers.barrier-components.unclaimed-dm-barrier.button.action"
				>
					{i18n._(CLAIM_ACCOUNT_DESCRIPTOR)}
				</Button>
			}
			data-flx="channel.barriers.barrier-components.unclaimed-dm-barrier.barrier-base"
		/>
	);
});
