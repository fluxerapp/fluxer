// SPDX-License-Identifier: AGPL-3.0-or-later

import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {ChannelHeader} from '@app/features/channel/components/ChannelHeader';
import {ChannelPageHeaderTitle} from '@app/features/channel/components/ChannelPageHeaderTitle';
import {ChannelViewScaffold} from '@app/features/channel/components/channel_view/ChannelViewScaffold';
import styles from '@app/features/guild/components/self_assignable_roles/SelfAssignableRoles.module.css';
import {SelfAssignableRolesView} from '@app/features/guild/components/self_assignable_roles/SelfAssignableRolesView';
import Guilds from '@app/features/guild/state/Guilds';
import {shouldShowSelfAssignableRolesEntry} from '@app/features/guild/utils/SelfAssignableRoles';
import {ROLES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Scroller} from '@app/features/ui/components/Scroller';
import {useFluxerDocumentTitle} from '@app/features/window/hooks/useFluxerDocumentTitle';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ShieldIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const ROLES_NOT_AVAILABLE_TITLE_DESCRIPTOR = msg({
	message: "There's nothing to pick here",
	comment:
		'Title of the Roles page when the community has turned off self-assignable roles or has none set up for members.',
});
const ROLES_NOT_AVAILABLE_DESCRIPTION_DESCRIPTOR = msg({
	message: "This community doesn't have any roles you can give yourself.",
	comment: 'Description of the Roles page when there are no self-assignable roles for the current member.',
});

interface SelfAssignableRolesPageProps {
	readonly guildId: string;
}

export const SelfAssignableRolesPage: React.FC<SelfAssignableRolesPageProps> = observer(
	function SelfAssignableRolesPage({guildId}) {
		const {i18n} = useLingui();
		const guild = Guilds.getGuild(guildId);
		const available = guild != null && shouldShowSelfAssignableRolesEntry(guild);
		useFluxerDocumentTitle(useMemo(() => [i18n._(ROLES_DESCRIPTOR), guild?.name], [guild?.name, i18n.locale]));
		const headerLeftContent = useMemo(
			() => (
				<ChannelPageHeaderTitle
					icon={ShieldIcon}
					label={i18n._(ROLES_DESCRIPTOR)}
					data-flx="guild.self-assignable-roles.self-assignable-roles-page.header-left-content.channel-page-header-title"
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
						data-flx="guild.self-assignable-roles.self-assignable-roles-page.channel-header"
					/>
				}
				chatArea={
					available ? (
						<Scroller
							className={styles.pageScroller}
							key={guildId}
							data-flx="guild.self-assignable-roles.self-assignable-roles-page.page-scroller"
						>
							<SelfAssignableRolesView
								guildId={guildId}
								data-flx="guild.self-assignable-roles.self-assignable-roles-page.self-assignable-roles-view"
							/>
						</Scroller>
					) : (
						<StatusSlate
							Icon={ShieldIcon}
							title={i18n._(ROLES_NOT_AVAILABLE_TITLE_DESCRIPTOR)}
							description={i18n._(ROLES_NOT_AVAILABLE_DESCRIPTION_DESCRIPTOR)}
							fullHeight={true}
							data-flx="guild.self-assignable-roles.self-assignable-roles-page.status-slate"
						/>
					)
				}
				data-flx="guild.self-assignable-roles.self-assignable-roles-page.channel-view-scaffold"
			/>
		);
	},
);
