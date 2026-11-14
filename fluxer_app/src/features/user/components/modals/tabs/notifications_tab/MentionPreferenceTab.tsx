// SPDX-License-Identifier: AGPL-3.0-or-later

import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import Users from '@app/features/user/state/Users';
import {type MentionReplyPreference, MentionReplyPreferences} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const REPLY_MENTION_PREFERENCE_DESCRIPTOR = msg({
	message: 'Reply mention preference',
	comment: 'Short label in the mention preference tab. Keep it concise.',
});
const NO_PREFERENCE_DESCRIPTOR = msg({
	message: 'No preference',
	comment: 'Empty-state text in the mention preference tab.',
});
const RESPECT_THE_SENDER_S_INTENT_WITH_NO_WARNING_DESCRIPTOR = msg({
	message: "Respect the sender's intent, with no warning when they toggle the @mention",
	comment: 'Warning text in the mention preference tab. Keep the tone plain and specific.',
});
const PREFER_MENTION_DESCRIPTOR = msg({
	message: 'Prefer @mention',
	comment: 'Short label in the mention preference tab. Keep it concise.',
});
const DEFAULT_REPLIES_TO_MENTION_YOU_AND_WARN_THE_DESCRIPTOR = msg({
	message: 'Default replies to @mention you, and warn the sender if they disable it',
	comment: 'Label in the mention preference tab.',
});
const PREFER_NO_MENTION_DESCRIPTOR = msg({
	message: 'Prefer no @mention',
	comment: 'Short label in the mention preference tab. Keep it concise.',
});
const DEFAULT_REPLIES_TO_OMIT_THE_MENTION_AND_WARN_DESCRIPTOR = msg({
	message: 'Default replies to omit the @mention, and warn the sender if they enable it',
	comment: 'Description text in the mention preference tab.',
});

type Value = 'no_preference' | 'prefer_mention' | 'prefer_no_mention';

function valueToFlag(value: Value): MentionReplyPreference {
	switch (value) {
		case 'prefer_mention':
			return MentionReplyPreferences.PREFER_MENTION;
		case 'prefer_no_mention':
			return MentionReplyPreferences.PREFER_NO_MENTION;
		default:
			return MentionReplyPreferences.NO_PREFERENCE;
	}
}

function flagToValue(flag: number): Value {
	if (flag === MentionReplyPreferences.PREFER_MENTION) return 'prefer_mention';
	if (flag === MentionReplyPreferences.PREFER_NO_MENTION) return 'prefer_no_mention';
	return 'no_preference';
}

export const MentionPreferenceTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const currentUser = Users.getCurrentUser();
	const current = flagToValue(currentUser?.mentionFlags ?? 0);
	const handleChange = async (value: Value) => {
		const mentionFlags = valueToFlag(value);
		const updatedUser = await UserCommands.update({mention_flags: mentionFlags});
		Users.handleUserUpdate({...updatedUser, mention_flags: mentionFlags}, {clearMissingOptionalFields: true});
	};
	return (
		<RadioGroup
			value={current}
			onChange={handleChange}
			aria-label={i18n._(REPLY_MENTION_PREFERENCE_DESCRIPTOR)}
			options={[
				{
					value: 'no_preference',
					name: i18n._(NO_PREFERENCE_DESCRIPTOR),
					desc: i18n._(RESPECT_THE_SENDER_S_INTENT_WITH_NO_WARNING_DESCRIPTOR),
				},
				{
					value: 'prefer_mention',
					name: i18n._(PREFER_MENTION_DESCRIPTOR),
					desc: i18n._(DEFAULT_REPLIES_TO_MENTION_YOU_AND_WARN_THE_DESCRIPTOR),
				},
				{
					value: 'prefer_no_mention',
					name: i18n._(PREFER_NO_MENTION_DESCRIPTOR),
					desc: i18n._(DEFAULT_REPLIES_TO_OMIT_THE_MENTION_AND_WARN_DESCRIPTOR),
				},
			]}
			data-flx="user.notifications-tab.mention-preference-tab.mention-preference-tab-content.radio-group.change"
		/>
	);
});
