// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AccountGateBarrier,
	AccountTooNewBarrier,
	DefaultBarrier,
	NotMemberLongEnoughBarrier,
	SendMessageDisabledBarrier,
	TimeoutBarrier,
} from '@app/features/channel/components/barriers/BarrierComponents';
import type {Channel} from '@app/features/channel/models/Channel';
import GuildVerification, {VerificationFailureReason} from '@app/features/guild/state/GuildVerification';
import {observer} from 'mobx-react-lite';

interface Props {
	channel: Channel;
}

export const VerificationBarrier = observer(({channel}: Props) => {
	const guildId = channel.guildId || '';
	const verificationStatus = GuildVerification.getVerificationStatus(guildId);
	if (!verificationStatus || verificationStatus.canAccess) {
		return null;
	}
	switch (verificationStatus.reason) {
		case VerificationFailureReason.ACCOUNT_GATE:
			return verificationStatus.accountGate ? (
				<AccountGateBarrier
					gate={verificationStatus.accountGate}
					data-flx="channel.verification-barrier.account-gate-barrier"
				/>
			) : (
				<DefaultBarrier data-flx="channel.verification-barrier.default-barrier" />
			);
		case VerificationFailureReason.ACCOUNT_TOO_NEW:
			return (
				<AccountTooNewBarrier
					initialTimeRemaining={
						verificationStatus.verificationEndsAt ? Math.max(0, verificationStatus.verificationEndsAt - Date.now()) : 0
					}
					data-flx="channel.verification-barrier.account-too-new-barrier"
				/>
			);
		case VerificationFailureReason.NOT_MEMBER_LONG_ENOUGH:
			return (
				<NotMemberLongEnoughBarrier
					initialTimeRemaining={
						verificationStatus.verificationEndsAt ? Math.max(0, verificationStatus.verificationEndsAt - Date.now()) : 0
					}
					data-flx="channel.verification-barrier.not-member-long-enough-barrier"
				/>
			);
		case VerificationFailureReason.SEND_MESSAGE_DISABLED:
			return <SendMessageDisabledBarrier data-flx="channel.verification-barrier.send-message-disabled-barrier" />;
		case VerificationFailureReason.TIMED_OUT:
			return (
				<TimeoutBarrier
					initialTimeRemaining={
						verificationStatus.verificationEndsAt ? Math.max(0, verificationStatus.verificationEndsAt - Date.now()) : 0
					}
					data-flx="channel.verification-barrier.timeout-barrier"
				/>
			);
		default:
			return <DefaultBarrier data-flx="channel.verification-barrier.default-barrier" />;
	}
});
