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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PhoneIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

import * as CallActionCreators from '~/actions/CallActionCreators';
import {useCallHeaderState} from '~/components/channel/channel-view/useCallHeaderState';
import {SystemMessage} from '~/components/channel/SystemMessage';
import {SystemMessageUsername} from '~/components/channel/SystemMessageUsername';
import {useSystemMessageData} from '~/hooks/useSystemMessageData';
import i18n from '~/i18n';
import type {MessageRecord} from '~/records/MessageRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import styles from './CallMessage.module.css';

type DurationUnit = 'year' | 'month' | 'week' | 'day' | 'hour' | 'minute';

const DURATION_UNITS: Array<{unit: DurationUnit; minutes: number}> = [
	{unit: 'year', minutes: 525600},
	{unit: 'month', minutes: 43800},
	{unit: 'week', minutes: 10080},
	{unit: 'day', minutes: 1440},
	{unit: 'hour', minutes: 60},
	{unit: 'minute', minutes: 1},
];

const LIST_FORMATTER =
	typeof Intl !== 'undefined' && typeof Intl.ListFormat !== 'undefined'
		? new Intl.ListFormat(undefined, {style: 'long', type: 'conjunction'})
		: null;

const formatLocalizedNumber = (value: number): string => {
	const locale = i18n.locale ?? 'en-US';
	return new Intl.NumberFormat(locale).format(value);
};

const replaceCountPlaceholder = (text: string, count: number): string => {
	if (!text.includes('#')) {
		return text;
	}
	return text.replace(/#/g, formatLocalizedNumber(count));
};

const DURATION_UNIT_LABELS: Record<DurationUnit, {singular: MessageDescriptor; plural: MessageDescriptor}> = {
	year: {singular: msg`1 year`, plural: msg`# years`},
	month: {singular: msg`1 month`, plural: msg`# months`},
	week: {singular: msg`1 week`, plural: msg`# weeks`},
	day: {singular: msg`1 day`, plural: msg`# days`},
	hour: {singular: msg`1 hour`, plural: msg`# hours`},
	minute: {singular: msg`a minute`, plural: msg`# minutes`},
};

const formatDurationUnit = (t: (message: MessageDescriptor) => string, value: number, unit: DurationUnit): string => {
	const descriptors = DURATION_UNIT_LABELS[unit] ?? DURATION_UNIT_LABELS.minute;
	const descriptor = value === 1 ? descriptors.singular : descriptors.plural;
	return replaceCountPlaceholder(t(descriptor), value);
};

const FEW_SECONDS_DESCRIPTOR = msg`a few seconds`;

const formatCallDuration = (t: (message: MessageDescriptor) => string, durationSeconds: number): string => {
	if (durationSeconds < 60) {
		return t(FEW_SECONDS_DESCRIPTOR);
	}

	const roundedMinutes = Math.max(1, Math.ceil(durationSeconds / 60));
	const parts: Array<string> = [];
	let remainingMinutes = roundedMinutes;

	for (const {unit, minutes} of DURATION_UNITS) {
		if (remainingMinutes < minutes) continue;
		const count = Math.floor(remainingMinutes / minutes);
		remainingMinutes -= count * minutes;
		parts.push(formatDurationUnit(t, count, unit));
	}

	if (parts.length === 0) {
		return t(DURATION_UNIT_LABELS.minute.singular);
	}

	if (!LIST_FORMATTER || parts.length === 1) {
		return parts.join(' ');
	}

	return LIST_FORMATTER.format(parts);
};

export const CallMessage = observer(({message}: {message: MessageRecord}) => {
	const {t} = useLingui();
	const {author, channel, guild} = useSystemMessageData(message);
	const currentUserId = AuthenticationStore.currentUserId;
	const callData = message.call;
	const isLocalConnected = channel ? MediaEngineStore.connected && MediaEngineStore.channelId === channel.id : false;
	const callHeaderState = useCallHeaderState(channel);
	const shouldShowJoinLink =
		!isLocalConnected &&
		!callHeaderState.isDeviceInRoomForChannelCall &&
		!callHeaderState.isDeviceConnectingToChannelCall &&
		callHeaderState.callExistsAndOngoing &&
		callHeaderState.controlsVariant === 'join';

	const handleJoinCall = useCallback(() => {
		if (!channel) return;
		CallActionCreators.joinCall(channel.id);
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
			? formatCallDuration(t, Math.max(0, (callData.endedTimestamp.getTime() - message.timestamp.getTime()) / 1000))
			: t`a few seconds`;

	let messageContent: React.ReactNode;
	if (!callEnded) {
		messageContent = (
			<>
				<Trans>
					<SystemMessageUsername key={author.id} author={author} guild={guild} message={message} /> started a call.
				</Trans>
				{shouldShowJoinLink && (
					<>
						<span className={styles.separator} aria-hidden="true">
							&nbsp;—&nbsp;
						</span>
						{/* biome-ignore lint/a11y/useValidAnchor: this is fine */}
						<a
							className={styles.callLink}
							href="#"
							onClick={(event) => {
								event.preventDefault();
								handleJoinCall();
							}}
						>
							{t`Join the call`}
						</a>
					</>
				)}
			</>
		);
	} else if (isMissedCall) {
		messageContent = durationText ? (
			<Trans>
				You missed a call from <SystemMessageUsername key={author.id} author={author} guild={guild} message={message} />{' '}
				that lasted {durationText}.
			</Trans>
		) : (
			<Trans>
				You missed a call from <SystemMessageUsername key={author.id} author={author} guild={guild} message={message} />
				.
			</Trans>
		);
	} else {
		messageContent = (
			<Trans>
				<SystemMessageUsername key={author.id} author={author} guild={guild} message={message} /> started a call that
				lasted {durationText}.
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
		/>
	);
});
