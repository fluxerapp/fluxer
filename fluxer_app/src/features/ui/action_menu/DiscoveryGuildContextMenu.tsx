// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import Guilds from '@app/features/guild/state/Guilds';
import {COPY_COMMUNITY_ID_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {ReportedGuild} from '@app/features/moderation/components/report_flow/ReportFlowModal';
import {REPORT_COMMUNITY_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import {openReportGuildModal} from '@app/features/moderation/utils/ReportActionUtils';
import {CopyIdIcon, ReportMessageIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface DiscoveryGuildContextMenuProps {
	guild: ReportedGuild;
	onClose: () => void;
}

export const DiscoveryGuildContextMenu: React.FC<DiscoveryGuildContextMenuProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const handleReport = useCallback(() => {
		onClose();
		openReportGuildModal({guild});
	}, [guild, onClose]);
	const handleCopyId = useCallback(() => {
		void TextCopyCommands.copy(i18n, guild.id);
		onClose();
	}, [guild.id, i18n, onClose]);
	const canReport = Guilds.getGuild(guild.id)?.isOwner(Authentication.currentUserId) !== true;
	return (
		<>
			{canReport && (
				<MenuGroup data-flx="ui.action-menu.discovery-guild-context-menu.menu-group.report">
					<MenuItem
						icon={<ReportMessageIcon data-flx="ui.action-menu.discovery-guild-context-menu.report-icon" />}
						onClick={handleReport}
						danger
						data-flx="ui.action-menu.discovery-guild-context-menu.menu-item.report"
					>
						{i18n._(REPORT_COMMUNITY_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			)}
			<MenuGroup data-flx="ui.action-menu.discovery-guild-context-menu.menu-group">
				<MenuItem
					icon={<CopyIdIcon data-flx="ui.action-menu.discovery-guild-context-menu.copy-id-icon" />}
					onClick={handleCopyId}
					data-flx="ui.action-menu.discovery-guild-context-menu.menu-item.copy-id"
				>
					{i18n._(COPY_COMMUNITY_ID_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
		</>
	);
});
