// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildList from '@app/features/guild/state/GuildList';
import Guilds from '@app/features/guild/state/Guilds';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {SealCheckIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const VERIFIED_COMMUNITY_DESCRIPTOR = msg({
	message: 'Verified community',
	comment: 'Badge or label indicating the community is verified.',
});
const DEFAULT_EMOJI_ON_PRODUCT_DESCRIPTOR = msg({
	message: 'This is a default emoji on {productName}.',
	comment: 'Emoji attribution text for built-in default emoji. productName is the Fluxer product name.',
});

type EmojiAttributionType = 'default' | 'custom_invite_required' | 'custom_unknown' | 'custom_guild';
type EmojiGuild = Guild | Guild;

export interface EmojiAttribution {
	type: EmojiAttributionType;
	guild?: EmojiGuild | null;
	isVerified?: boolean;
}

export interface EmojiAttributionSource {
	emojiId?: string | null;
	guildId?: string | null;
	guild?: EmojiGuild | null;
	emojiName?: string | null;
}

const getIsVerified = (guild?: EmojiGuild | null): boolean => {
	if (!guild) return false;
	const features = guild.features;
	if (!features) return false;
	if (Array.isArray(features)) {
		return features.includes('VERIFIED');
	}
	if (features instanceof Set) {
		return features.has('VERIFIED');
	}
	return false;
};
export const getEmojiAttribution = ({emojiId, guildId, guild}: EmojiAttributionSource): EmojiAttribution => {
	if (!emojiId) {
		return {type: 'default'};
	}
	const resolvedGuild = guildId ? (guild ?? Guilds.getGuild(guildId)) : null;
	const isVerified = getIsVerified(resolvedGuild);
	if (resolvedGuild) {
		return {type: 'custom_guild', guild: resolvedGuild, isVerified};
	}
	const isMember = guildId ? GuildList.guilds.some((candidate) => candidate.id === guildId) : null;
	if (isMember === false) {
		return {type: 'custom_invite_required'};
	}
	return {type: 'custom_unknown'};
};

interface EmojiAttributionSubtextProps {
	attribution: EmojiAttribution;
	classes?: {
		container?: string;
		text?: string;
		guildRow?: string;
		guildIcon?: string;
		guildName?: string;
		verifiedIcon?: string;
	};
}

export const EmojiAttributionSubtext = observer(function EmojiAttributionSubtext({
	attribution,
	classes = {},
}: EmojiAttributionSubtextProps) {
	const {i18n} = useLingui();
	if (attribution.type === 'default') {
		return (
			<div className={classes.container} data-flx="emoji.emojis.emoji-attribution-subtext.div">
				<span className={classes.text} data-flx="emoji.emojis.emoji-attribution-subtext.span">
					{i18n._(DEFAULT_EMOJI_ON_PRODUCT_DESCRIPTOR, {productName: PRODUCT_NAME})}
				</span>
			</div>
		);
	}
	if (attribution.type === 'custom_invite_required') {
		return (
			<div className={classes.container} data-flx="emoji.emojis.emoji-attribution-subtext.div--2">
				<span className={classes.text} data-flx="emoji.emojis.emoji-attribution-subtext.span--2">
					<Trans>This is a custom emoji from a community. Ask the author for an invite to use this emoji.</Trans>
				</span>
			</div>
		);
	}
	if (attribution.type === 'custom_unknown' || !attribution.guild) {
		return (
			<div className={classes.container} data-flx="emoji.emojis.emoji-attribution-subtext.div--3">
				<span className={classes.text} data-flx="emoji.emojis.emoji-attribution-subtext.span--3">
					<Trans>This is a custom emoji from a community.</Trans>
				</span>
			</div>
		);
	}
	return (
		<div className={classes.container} data-flx="emoji.emojis.emoji-attribution-subtext.div--4">
			<span className={classes.text} data-flx="emoji.emojis.emoji-attribution-subtext.span--4">
				<Trans>This is a custom emoji from</Trans>
			</span>
			<div className={classes.guildRow} data-flx="emoji.emojis.emoji-attribution-subtext.div--5">
				<div className={classes.guildIcon} data-flx="emoji.emojis.emoji-attribution-subtext.div--6">
					<GuildIcon
						id={attribution.guild.id}
						name={attribution.guild.name}
						icon={attribution.guild.icon}
						sizePx={20}
						data-flx="emoji.emojis.emoji-attribution-subtext.guild-icon"
					/>
				</div>
				<span className={classes.guildName} data-flx="emoji.emojis.emoji-attribution-subtext.span--5">
					{attribution.guild.name}
				</span>
				{attribution.isVerified && (
					<Tooltip
						text={i18n._(VERIFIED_COMMUNITY_DESCRIPTOR)}
						position="top"
						data-flx="emoji.emojis.emoji-attribution-subtext.tooltip"
					>
						<SealCheckIcon
							className={classes.verifiedIcon}
							data-flx="emoji.emojis.emoji-attribution-subtext.seal-check-icon"
						/>
					</Tooltip>
				)}
			</div>
		</div>
	);
});

EmojiAttributionSubtext.displayName = 'EmojiAttributionSubtext';
