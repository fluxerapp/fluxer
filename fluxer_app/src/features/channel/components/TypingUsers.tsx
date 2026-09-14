// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getTypingText as getLegacyTypingText,
	TypingUsers as LegacyTypingUsers,
	usePresentableTypingUsers as legacyUsePresentableTypingUsers,
} from '@app/features/channel/components/LegacyTypingUsers';
import {
	RollingTypingAnnouncer,
	RollingTypingUsers,
	rollingUsePresentableTypingUsers,
} from '@app/features/channel/components/RollingTypingUsers';
import type {Channel} from '@app/features/channel/models/Channel';
import {getRollingTypingText} from '@app/features/typing/rolling/RollingTypingText';
import TypingPolicy from '@app/features/typing/state/TypingPolicy';
import type {User} from '@app/features/user/models/User';
import type {I18n} from '@lingui/core';
import {observer} from 'mobx-react-lite';
import type {ReactNode, RefObject} from 'react';

interface TypingUsersProps {
	channel: Channel;
	withText?: boolean;
	showAvatars?: boolean;
	overflowContainerRef?: RefObject<HTMLElement | null>;
}

export const getTypingText = (i18n: I18n, typingUsers: ReadonlyArray<User>, channel: Channel): ReactNode =>
	TypingPolicy.active === 'rolling'
		? getRollingTypingText(i18n, typingUsers, channel, false)
		: getLegacyTypingText(i18n, typingUsers, channel);

export const usePresentableTypingUsers = (channel: Channel): ReadonlyArray<User> =>
	TypingPolicy.active === 'rolling'
		? rollingUsePresentableTypingUsers(channel)
		: legacyUsePresentableTypingUsers(channel);

export const TypingUsers = observer(({channel, withText, showAvatars, overflowContainerRef}: TypingUsersProps) =>
	TypingPolicy.active === 'rolling' ? (
		<RollingTypingUsers
			channel={channel}
			withText={withText}
			showAvatars={showAvatars}
			overflowContainerRef={overflowContainerRef}
			data-flx="channel.typing-users.rolling-typing-users"
		/>
	) : (
		<LegacyTypingUsers
			channel={channel}
			withText={withText}
			showAvatars={showAvatars}
			data-flx="channel.typing-users.legacy-typing-users"
		/>
	),
);

export const TypingAnnouncer = observer(({channel}: {channel: Channel}) =>
	TypingPolicy.active === 'rolling' ? (
		<RollingTypingAnnouncer channel={channel} data-flx="channel.typing-users.rolling-typing-announcer" />
	) : null,
);
