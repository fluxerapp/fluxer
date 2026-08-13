// SPDX-License-Identifier: AGPL-3.0-or-later

import GuildMembers from '@app/features/member/state/GuildMembers';
import Permission from '@app/features/permissions/state/Permission';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/theme/styles/Message.module.css';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ClockIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const TIMED_OUT_DESCRIPTOR = msg({
	message: 'Timed out',
	comment: 'Short label in the channel and chat message timeout indicator. Keep it concise.',
});
const TIMEOUT_UNTIL_DESCRIPTOR = msg({
	message: 'Timeout until {timeoutDate}',
	comment:
		'Tooltip on the message username timeout indicator. {timeoutDate} is the formatted date and time when the timeout ends.',
});

interface MessageTimeoutIndicatorProps {
	guildId: string | null | undefined;
	userId: string | null | undefined;
}

export const MessageTimeoutIndicator = observer(({guildId, userId}: MessageTimeoutIndicatorProps) => {
	const {i18n} = useLingui();
	const normalizedGuildId = guildId ?? null;
	if (!normalizedGuildId || !Permission.can(Permissions.MODERATE_MEMBERS, {guildId: normalizedGuildId})) return null;
	const timeoutUntil = GuildMembers.getCommunicationDisabledUntil(normalizedGuildId, userId);
	if (!timeoutUntil) return null;
	const timeoutDate = DateUtils.getFormattedDateTime(timeoutUntil);
	return (
		<Tooltip
			text={i18n._(TIMEOUT_UNTIL_DESCRIPTOR, {timeoutDate})}
			position="top"
			maxWidth="none"
			data-flx="channel.message-timeout-indicator.tooltip"
		>
			<span
				className={styles.messageTimeoutIndicator}
				role="img"
				aria-label={i18n._(TIMED_OUT_DESCRIPTOR)}
				data-flx="channel.message-timeout-indicator.message-timeout-indicator"
			>
				<ClockIcon size={remFromPx(16)} weight="bold" data-flx="channel.message-timeout-indicator.clock-icon" />
			</span>
		</Tooltip>
	);
});
