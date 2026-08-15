// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import * as ChannelDisplayUtils from '@app/features/channel/utils/ChannelDisplayUtils';
import Guilds from '@app/features/guild/state/Guilds';
import {ComposerMentionContext} from '@app/features/lexical/composer/ComposerMentionContext';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import type {ComposerMentionType} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {MentionWithTooltip} from '@app/features/lexical/composer/nodes/MentionTooltipContent';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import mentionRendererStyles from '@app/features/theme/styles/MentionRenderer.module.css';
import Users from '@app/features/user/state/Users';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useContext} from 'react';

const FULL_USER_TAG_DESCRIPTOR = msg({
	message: 'Full user tag: {tag}',
	comment:
		'Screen-reader-only identity detail appended to a user mention in the message composer. Preserve {tag}; it contains the full user tag.',
});
const UNKNOWN_ROLE_DESCRIPTOR = msg({
	message: 'Unknown role',
	comment: 'Fallback label for a role mention whose role no longer exists, in the message composer. Keep it concise.',
});

interface ComposerMentionPillProps {
	mentionType: ComposerMentionType;
	mentionId: string;
	display: string;
}

export const ComposerMentionPill = observer(({mentionType, mentionId, display}: ComposerMentionPillProps) => {
	const {guildId, channelId, plainText} = useContext(ComposerMentionContext);
	const {i18n} = useLingui();
	if (plainText) {
		return (
			<span className={styles.plainText} contentEditable={false}>
				{display}
			</span>
		);
	}

	if (mentionType === 'channel') {
		const channel = Channels.getChannel(mentionId);
		const label = channel == null || channel.name == null ? display.replace(/^#/, '') : channel.name;
		if (channel != null && channel.type === ChannelTypes.GUILD_CATEGORY) {
			return (
				<span contentEditable={false} data-lexical-mention-type="channel">
					#{label}
				</span>
			);
		}
		return (
			<span className={markupStyles.mention} contentEditable={false} data-lexical-mention-type="channel">
				{ChannelDisplayUtils.getIcon(channel == null ? {type: ChannelTypes.GUILD_TEXT} : channel, {
					className: mentionRendererStyles.channelIcon,
				})}
				<span className={mentionRendererStyles.label}>{label}</span>
			</span>
		);
	}

	if (mentionType === 'user') {
		const user = Users.getUser(mentionId);
		const label = user ? `@${DisplayNameUtils.getNickname(user, guildId, channelId)}` : display;
		const fullTag = user ? `@${DisplayNameUtils.formatUserTagForStreamerMode(user)}` : null;
		const pill = (
			<span className={markupStyles.mention} contentEditable={false} data-lexical-mention-type="user">
				<span className={mentionRendererStyles.label}>{label}</span>
				{fullTag != null && fullTag !== label && (
					<span className={styles.srOnly}>{i18n._(FULL_USER_TAG_DESCRIPTOR, {tag: fullTag})}</span>
				)}
			</span>
		);
		if (!user) {
			return pill;
		}
		return (
			<MentionWithTooltip userId={mentionId} guildId={guildId} channelId={channelId}>
				{pill}
			</MentionWithTooltip>
		);
	}

	if (mentionType === 'role') {
		const role = guildId != null ? Guilds.getGuildRole(guildId, mentionId) : undefined;
		const label = role ? `@${role.name}` : `@${i18n._(UNKNOWN_ROLE_DESCRIPTOR)}`;
		return (
			<span className={markupStyles.mention} contentEditable={false} data-lexical-mention-type="role">
				<span className={mentionRendererStyles.label}>{label}</span>
			</span>
		);
	}

	return (
		<span className={markupStyles.mention} contentEditable={false} data-lexical-mention-type={mentionType}>
			<span className={mentionRendererStyles.label}>{display}</span>
		</span>
	);
});
