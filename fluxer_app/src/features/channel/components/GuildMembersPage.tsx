// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelHeader} from '@app/features/channel/components/ChannelHeader';
import {ChannelViewScaffold} from '@app/features/channel/components/channel_view/ChannelViewScaffold';
import styles from '@app/features/channel/components/GuildMembersPage.module.css';
import {MembersTableView} from '@app/features/channel/components/guild_members_page/MembersTableView';
import Guilds from '@app/features/guild/state/Guilds';
import {MEMBERS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {useLingui} from '@lingui/react/macro';
import {UsersIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

interface GuildMembersPageProps {
	guildId: string;
}

export const GuildMembersPage: React.FC<GuildMembersPageProps> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	useFluxerDocumentTitle(useMemo(() => [i18n._(MEMBERS_DESCRIPTOR), guild?.name], [guild?.name, i18n.locale]));
	const headerLeftContent = useMemo(
		() => (
			<div
				className={styles.headerLeftContent}
				data-flx="channel.guild-members-page.header-left-content.header-left-content"
			>
				<UsersIcon
					className={styles.headerIcon}
					size={remFromPx(20)}
					data-flx="channel.guild-members-page.header-left-content.header-icon"
				/>
				<span className={styles.headerLabel} data-flx="channel.guild-members-page.header-left-content.header-label">
					{i18n._(MEMBERS_DESCRIPTOR)}
				</span>
			</div>
		),
		[i18n.locale],
	);
	return (
		<ChannelViewScaffold
			header={
				<ChannelHeader
					leftContent={headerLeftContent}
					showMembersToggle={false}
					showPins={false}
					data-flx="channel.guild-members-page.channel-header"
				/>
			}
			chatArea={<MembersTableView guildId={guildId} data-flx="channel.guild-members-page.members-table-view" />}
			data-flx="channel.guild-members-page.channel-view-scaffold"
		/>
	);
});
