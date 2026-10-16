// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/components/MobileGuildSettingsView.module.css';
import {MobileEntitySettingsView} from '@app/features/app/components/dialogs/shared/MobileEntitySettingsView';
import {MobileSettingsDangerItem} from '@app/features/app/components/dialogs/shared/MobileSettingsComponents';

import Authentication from '@app/features/auth/state/Authentication';
import {GuildDeleteModal} from '@app/features/guild/components/modals/GuildDeleteModal';
import type {Guild} from '@app/features/guild/models/Guild';
import {isStockCommunityGuild} from '@app/features/guild/utils/GuildCommunityUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';

import type {
	GuildSettingsTab,
	GuildSettingsTabType,
} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {getGuildSettingsCategoryLabel} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import {getGuildSettingsTabComponent} from '@app/features/user/components/settings_utils/GuildSettingsTabComponents';
import type {MobileNavigationState} from '@app/features/user/hooks/useMobileNavigation';

import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';

import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const DELETE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Delete community',
	comment: 'Short label in the settings dialog mobile guild settings view. Keep the tone plain and specific.',
});

interface MobileGuildSettingsViewProps {
	guild: Guild;
	groupedSettingsTabs: Record<string, Array<GuildSettingsTab>>;
	currentTab?: GuildSettingsTab;
	mobileNav: MobileNavigationState<GuildSettingsTabType>;
	onBack: () => void;
	onTabSelect: (tabType: string, title: string) => void;
}

export const MobileGuildSettingsView: React.FC<MobileGuildSettingsViewProps> = observer(
	({guild, groupedSettingsTabs, currentTab, mobileNav, onBack, onTabSelect}) => {
		const {i18n} = useLingui();
		const CurrentTabComponent = currentTab ? getGuildSettingsTabComponent(currentTab.type) : null;
		const CATEGORY_LABELS = {
			expressions: getGuildSettingsCategoryLabel(i18n, 'expressions'),
			community: getGuildSettingsCategoryLabel(i18n, 'community'),
			integrations: getGuildSettingsCategoryLabel(i18n, 'integrations'),
			user_management: getGuildSettingsCategoryLabel(i18n, 'user_management'),
		};
		const handleDeleteGuild = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<GuildDeleteModal
						guildId={guild.id}
						data-flx="app.mobile-guild-settings-view.handle-delete-guild.guild-delete-modal"
					/>
				)),
			);
		}, [guild.id]);
		const dangerAction =
			guild.isOwner(Authentication.currentUserId) && !isStockCommunityGuild(guild.id) ? (
				<MobileSettingsDangerItem
					icon={TrashIcon}
					label={i18n._(DELETE_COMMUNITY_DESCRIPTOR)}
					onClick={handleDeleteGuild}
					data-flx="app.mobile-guild-settings-view.mobile-settings-danger-item.delete-guild"
				/>
			) : null;
		return (
			<MobileEntitySettingsView
				groupedSettingsTabs={groupedSettingsTabs}
				currentTab={currentTab}
				mobileNav={mobileNav}
				onBack={onBack}
				onTabSelect={onTabSelect}
				rootTitle={guild.name}
				mobileTitle={currentTab?.label || mobileNav.currentView?.title}
				content={
					CurrentTabComponent ? (
						<CurrentTabComponent guildId={guild.id} data-flx="app.mobile-guild-settings-view.current-tab-component" />
					) : null
				}
				dangerAction={dangerAction}
				categoryLabels={CATEGORY_LABELS}
				hiddenCategories={['guild_settings']}
				contentScrollerClassName={styles.scrollerFlex}
				contentInnerClassName={styles.contentContainer}
				contentScrollerKey="mobile-guild-settings-content-scroller"
				data-flx="app.mobile-guild-settings-view.mobile-entity-settings-view"
			/>
		);
	},
);
