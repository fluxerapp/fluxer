// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import styles from '@app/features/channel/components/CallMessage.module.css';
import {useCallHeaderState} from '@app/features/channel/components/channel_view/useCallHeaderState';
import {SystemMessage} from '@app/features/channel/components/SystemMessage';
import {SystemMessageUsername} from '@app/features/channel/components/SystemMessageUsername';
import {
	ONE_DAY_DURATION_DESCRIPTOR,
	ONE_HOUR_DURATION_DESCRIPTOR,
	ONE_MONTH_DURATION_DESCRIPTOR,
	ONE_WEEK_DURATION_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useSystemMessageData} from '@app/features/messaging/hooks/useSystemMessageData';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import * as CallCommands from '@app/features/voice/commands/CallCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PhoneIcon} from '@phosphor-icons/react';
import {formatListWithConfig} from '@pkgs/list_utils/src/ListFormatting';
import {formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const MESSAGE_1_YEAR_DESCRIPTOR = msg({
	message: '1 year',
	comment: 'Duration label component for an elapsed call. Shown when the call lasted about one year.',
});
const A_MINUTE_DESCRIPTOR = msg({
	message: 'a minute',
	comment: 'Duration label component for an elapsed call. Shown when the call lasted about a minute.',
});
const YEARS_DESCRIPTOR = msg({
	message: '{count, plural, one {{countLabel} year} other {{countLabel} years}}',
	comment: 'Duration label component for an elapsed call. countLabel is a formatted count.',
});
const MONTHS_DESCRIPTOR = msg({
	message: '{count, plural, one {{countLabel} month} other {{countLabel} months}}',
	comment: 'Duration label component for an elapsed call. countLabel is a formatted count.',
});
const WEEKS_DESCRIPTOR = msg({
	message: '{count, plural, one {{countLabel} week} other {{countLabel} weeks}}',
	comment: 'Duration label component for an elapsed call. countLabel is a formatted count.',
});
const DAYS_DESCRIPTOR = msg({
	message: '{count, plural, one {{countLabel} day} other {{countLabel} days}}',
	comment: 'Duration label component for an elapsed call. countLabel is a formatted count.',
});
const HOURS_DESCRIPTOR = msg({
	message: '{count, plural, one {{countLabel} hour} other {{countLabel} hours}}',
	comment: 'Duration label component for an elapsed call. countLabel is a formatted count.',
});
const MINUTES_DESCRIPTOR = msg({
	message: '{count, plural, one {{countLabel} minute} other {{countLabel} minutes}}',
	comment: 'Duration label component for an elapsed call. countLabel is a formatted count.',
});
const A_FEW_SECONDS_DESCRIPTOR = msg({
	message: 'a few seconds',
	comment: 'Duration label component for a very short elapsed call.',
});
const JOIN_THE_CALL_DESCRIPTOR = msg({
	message: 'Join the call',
	comment: 'Call-to-action button label on an in-progress call system message.',
});

type DurationUnit = 'year' | 'month' | 'week' | 'day' | 'hour' | 'minute';

const DURATION_UNITS: Array<{unit: DurationUnit; minutes: number}> = [
	{unit: 'year', minutes: 525600},
	{unit: 'month', minutes: 43800},
	{unit: 'week', minutes: 10080},
	{unit: 'day', minutes: 1440},
	{unit: 'hour', minutes: 60},
	{unit: 'minute', minutes: 1},
];
const formatLocalizedNumber = (value: number): string => {
	const locale = getCurrentLocale();
	return formatNumber(value, locale);
};
const formatDurationUnit = (i18n: I18n, value: number, unit: DurationUnit): string => {
	if (value === 1) {
		switch (unit) {
			case 'year':
				return i18n._(MESSAGE_1_YEAR_DESCRIPTOR);
			case 'month':
				return i18n._(ONE_MONTH_DURATION_DESCRIPTOR);
			case 'week':
				return i18n._(ONE_WEEK_DURATION_DESCRIPTOR);
			case 'day':
				return i18n._(ONE_DAY_DURATION_DESCRIPTOR);
			case 'hour':
				return i18n._(ONE_HOUR_DURATION_DESCRIPTOR);
			default:
				return i18n._(A_MINUTE_DESCRIPTOR);
		}
	}
	const countLabel = formatLocalizedNumber(value);
	switch (unit) {
		case 'year':
			return i18n._(YEARS_DESCRIPTOR, {countLabel, count: value});
		case 'month':
			return i18n._(MONTHS_DESCRIPTOR, {countLabel, count: value});
		case 'week':
			return i18n._(WEEKS_DESCRIPTOR, {countLabel, count: value});
		case 'day':
			return i18n._(DAYS_DESCRIPTOR, {countLabel, count: value});
		case 'hour':
			return i18n._(HOURS_DESCRIPTOR, {countLabel, count: value});
		default:
			return i18n._(MINUTES_DESCRIPTOR, {countLabel, count: value});
	}
};
const FEW_SECONDS_DESCRIPTOR = A_FEW_SECONDS_DESCRIPTOR;
const formatCallDuration = (i18n: I18n, durationSeconds: number): string => {
	if (durationSeconds < 60) {
		return i18n._(FEW_SECONDS_DESCRIPTOR);
	}
	const roundedMinutes = Math.max(1, Math.ceil(durationSeconds / 60));
	const parts: Array<string> = [];
	let remainingMinutes = roundedMinutes;
	for (const {unit, minutes} of DURATION_UNITS) {
		if (remainingMinutes < minutes) continue;
		const count = Math.floor(remainingMinutes / minutes);
		remainingMinutes -= count * minutes;
		parts.push(formatDurationUnit(i18n, count, unit));
	}
	if (parts.length === 0) {
		return i18n._(A_MINUTE_DESCRIPTOR);
	}
	const locale = getCurrentLocale();
	return formatListWithConfig(parts, {locale, style: 'long', type: 'conjunction'});
};
export const CallMessage = observer(({message}: {message: Message}) => {
	const {i18n} = useLingui();
	const {author, channel, guild} = useSystemMessageData(message);
	const currentUserId = Authentication.currentUserId;
	const callData = message.call;
	const isLocalConnected = channel ? MediaEngine.connected && MediaEngine.channelId === channel.id : false;
	const callHeaderState = useCallHeaderState(channel);
	const shouldShowJoinLink =
		!isLocalConnected &&
		!callHeaderState.isDeviceInRoomForChannelCall &&
		!callHeaderState.isDeviceConnectingToChannelCall &&
		callHeaderState.callExistsAndOngoing &&
		callHeaderState.controlsVariant === 'join';
	const handleJoinCall = useCallback(() => {
		if (!channel) return;
		CallCommands.joinCall(channel.id);
	}, [channel]);
	if (!channel || !callData) {
		return null;
	}
	const callEnded = callData.endedTimestamp != null;
	const participantIds = callData.participants;
	const includesCurrentUser = Boolean(currentUserId && participantIds.includes(currentUserId));
	const authorIsCurrentUser = author.id === currentUserId;
	const isMissedCall = callEnded && !includesCurrentUser && !authorIsCurrentUser;
	const durationText =
		callEnded && callData.endedTimestamp
			? formatCallDuration(i18n, Math.max(0, (callData.endedTimestamp.getTime() - message.timestamp.getTime()) / 1000))
			: i18n._(A_FEW_SECONDS_DESCRIPTOR);
	let messageContent: React.ReactNode;
	if (!callEnded) {
		messageContent = (
			<>
				<Trans>
					<SystemMessageUsername
						key={author.id}
						author={author}
						guild={guild}
						message={message}
						data-flx="channel.call-message.system-message-username"
					/>{' '}
					started a call.
				</Trans>
				{shouldShowJoinLink && (
					<>
						{' '}
						<FocusRing offset={-2} data-flx="channel.call-message.focus-ring">
							<button
								type="button"
								className={styles.callLink}
								onClick={handleJoinCall}
								data-flx="channel.call-message.call-link.join-call.button"
							>
								{i18n._(JOIN_THE_CALL_DESCRIPTOR)}
							</button>
						</FocusRing>
					</>
				)}
			</>
		);
	} else if (isMissedCall) {
		messageContent = durationText ? (
			<Trans>
				You missed a call from{' '}
				<SystemMessageUsername
					key={author.id}
					author={author}
					guild={guild}
					message={message}
					data-flx="channel.call-message.system-message-username--2"
				/>{' '}
				that lasted {durationText}.
			</Trans>
		) : (
			<Trans>
				You missed a call from{' '}
				<SystemMessageUsername
					key={author.id}
					author={author}
					guild={guild}
					message={message}
					data-flx="channel.call-message.system-message-username--3"
				/>
				.
			</Trans>
		);
	} else {
		messageContent = (
			<Trans>
				<SystemMessageUsername
					key={author.id}
					author={author}
					guild={guild}
					message={message}
					data-flx="channel.call-message.system-message-username--4"
				/>{' '}
				started a call that lasted {durationText}.
			</Trans>
		);
	}
	const iconClassname = clsx(
		styles.icon,
		callEnded ? (isMissedCall ? styles.iconMissed : styles.iconEnded) : styles.iconActive,
	);
	return (
		<SystemMessage
			icon={PhoneIcon}
			iconWeight="fill"
			iconClassname={iconClassname}
			message={message}
			messageContent={messageContent}
			data-flx="channel.call-message.system-message"
		/>
	);
});
