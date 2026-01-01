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

import {Trans, useLingui} from '@lingui/react/macro';
import {SealCheckIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {GuildFeatures} from '~/Constants';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {Emoji} from '~/stores/EmojiStore';
import GuildListStore from '~/stores/GuildListStore';
import GuildStore from '~/stores/GuildStore';
import styles from './EmojiInfoContent.module.css';

interface EmojiInfoContentProps {
	emoji: Emoji;
}

export const EmojiInfoContent = observer(function EmojiInfoContent({emoji}: EmojiInfoContentProps) {
	const {t} = useLingui();
	const isCustomEmoji = Boolean(emoji.guildId || emoji.id);

	if (!isCustomEmoji) {
		return (
			<div className={styles.container}>
				<span className={styles.text}>
					<Trans>This is a default emoji on Fluxer.</Trans>
				</span>
			</div>
		);
	}

	const guildId = emoji.guildId;
	const isMember = guildId ? GuildListStore.guilds.some((guild) => guild.id === guildId) : false;

	if (!isMember) {
		return (
			<div className={styles.container}>
				<span className={styles.text}>
					<Trans>This is a custom emoji from a community. Ask the author for an invite to use this emoji.</Trans>
				</span>
			</div>
		);
	}

	const guild = guildId ? GuildStore.getGuild(guildId) : null;

	if (!guild) {
		return (
			<div className={styles.container}>
				<span className={styles.text}>
					<Trans>This is a custom emoji from a community.</Trans>
				</span>
			</div>
		);
	}

	const isVerified = guild.features.has(GuildFeatures.VERIFIED);

	return (
		<div className={styles.container}>
			<span className={styles.text}>
				<Trans>This is a custom emoji from</Trans>
			</span>
			<div className={styles.guildRow}>
				<div className={styles.guildIcon}>
					<GuildIcon id={guild.id} name={guild.name} icon={guild.icon} sizePx={20} />
				</div>
				<span className={styles.guildName}>{guild.name}</span>
				{isVerified && (
					<Tooltip text={t`Verified Community`} position="top">
						<SealCheckIcon className={styles.verifiedIcon} />
					</Tooltip>
				)}
			</div>
		</div>
	);
});
