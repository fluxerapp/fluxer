// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/TypingUsers.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {TYPING_ROLLING_MAX_NAMES} from '@app/features/typing/rolling/TypingSendThrottle';
import {getTypingTierText} from '@app/features/typing/utils/TypingTierText';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import type {ReactNode} from 'react';

const ONE_TYPIST_DESCRIPTOR = msg({
	message: '{a} is typing...',
	comment: 'Screen reader announcement in the composer when one person is typing. {a} is their display name.',
});
const TWO_TYPISTS_DESCRIPTOR = msg({
	message: '{a} and {b} are typing...',
	comment:
		'Screen reader announcement in the composer when two people are typing. {a} and {b} are their display names.',
});
const THREE_TYPISTS_DESCRIPTOR = msg({
	message: '{a}, {b} and {c} are typing...',
	comment:
		'Screen reader announcement in the composer when three people are typing. {a}, {b} and {c} are their display names.',
});
const MULTIPLE_PEOPLE_ARE_TYPING_DESCRIPTOR = msg({
	message: 'Multiple people are typing...',
	comment: 'Label in the composer typing row when the names of the people typing do not fit.',
});

function getTypistName(user: User, channel: Channel): string {
	return NicknameUtils.getNickname(user, channel.guildId ?? null);
}

function getTypingNames(typingUsers: ReadonlyArray<User>, channel: Channel): ReactNode {
	const [a, b, c] = typingUsers.slice(0, TYPING_ROLLING_MAX_NAMES).map((user) => (
		<span
			key={user.id}
			className={styles.username}
			style={{color: GuildMembers.getMember(channel.guildId ?? '', user.id)?.getColorString()}}
			data-flx="channel.typing-users.get-typing-text.username"
		>
			{getTypistName(user, channel)}
		</span>
	));
	if (typingUsers.length === 1) {
		return <Trans>{a} is typing...</Trans>;
	}
	if (typingUsers.length === 2) {
		return (
			<Trans>
				{a} and {b} are typing...
			</Trans>
		);
	}
	return (
		<Trans>
			{a}, {b} and {c} are typing...
		</Trans>
	);
}

export function getRollingTypingText(
	i18n: I18n,
	typingUsers: ReadonlyArray<User>,
	channel: Channel,
	overflowing: boolean,
): ReactNode {
	if (typingUsers.length === 0) {
		return null;
	}
	if (typingUsers.length > TYPING_ROLLING_MAX_NAMES) {
		return getTypingTierText(i18n, typingUsers.length);
	}
	if (overflowing) {
		return i18n._(MULTIPLE_PEOPLE_ARE_TYPING_DESCRIPTOR);
	}
	return getTypingNames(typingUsers, channel);
}

export function getRollingTypingAnnouncement(i18n: I18n, typingUsers: ReadonlyArray<User>, channel: Channel): string {
	if (typingUsers.length === 0) {
		return '';
	}
	if (typingUsers.length > TYPING_ROLLING_MAX_NAMES) {
		return getTypingTierText(i18n, typingUsers.length);
	}
	const [a, b, c] = typingUsers.map((user) => getTypistName(user, channel));
	if (typingUsers.length === 1) {
		return i18n._(ONE_TYPIST_DESCRIPTOR, {a});
	}
	if (typingUsers.length === 2) {
		return i18n._(TWO_TYPISTS_DESCRIPTOR, {a, b});
	}
	return i18n._(THREE_TYPISTS_DESCRIPTOR, {a, b, c});
}
