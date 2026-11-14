// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelHeader} from '@app/features/channel/components/ChannelHeader';
import {ChannelPageHeaderTitle} from '@app/features/channel/components/ChannelPageHeaderTitle';
import {ChannelViewScaffold} from '@app/features/channel/components/channel_view/ChannelViewScaffold';
import {MembersTableView} from '@app/features/channel/components/guild_members_page/MembersTableView';
import Guilds from '@app/features/guild/state/Guilds';
import {MEMBERS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Users from '@app/features/user/state/Users';
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
	const viewAccountKey = Users.viewAccountKey;
	useFluxerDocumentTitle(useMemo(() => [i18n._(MEMBERS_DESCRIPTOR), guild?.name], [guild?.name, i18n.locale]));
	const headerLeftContent = useMemo(
		() => (
			<ChannelPageHeaderTitle
				icon={UsersIcon}
				label={i18n._(MEMBERS_DESCRIPTOR)}
				data-flx="channel.guild-members-page.header-left-content.channel-page-header-title"
			/>
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
			chatArea={
				<MembersTableView
					key={viewAccountKey}
					guildId={guildId}
					data-flx="channel.guild-members-page.members-table-view"
				/>
			}
			data-flx="channel.guild-members-page.channel-view-scaffold"
		/>
	);
});
