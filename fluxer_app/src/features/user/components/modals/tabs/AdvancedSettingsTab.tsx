// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSearch} from '@app/features/app/components/dialogs/components/SettingsSearch';
import {SettingsHeadingLinkButton} from '@app/features/app/components/dialogs/shared/SettingsHeadingLinkButton';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import styles from '@app/features/user/components/modals/tabs/AdvancedSettingsTab.module.css';
import {AdvancedSettingRow} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedSettingRow';
import {
	ADVANCED_SETTINGS_TAG_LABELS,
	ADVANCED_SETTINGS_TAG_ORDER,
	getAdvancedSettingsCategorySectionId,
	groupAdvancedItemsByCategory,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedSettingsCategories';
import {
	getAdvancedSettingSourceSection,
	getAdvancedSettingSourceTab,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedSettingsItemUtils';
import {
	isSettingsItemNew,
	type SettingsCategoryTag,
} from '@app/features/user/components/settings_utils/SettingsMetadata';
import {
	filterAdvancedSettingItems,
	getAdvancedSettingItems,
} from '@app/features/user/components/settings_utils/SettingsSearchIndex';
import type {SearchableSettingItem} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {SettingsStatusBadge} from '@app/features/user/components/settings_utils/SettingsStatusBadge';
import {buildUserSettingsDeepLink} from '@app/features/user/components/settings_utils/UserSettingsDeepLinks';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo, useState} from 'react';

const SEARCH_ADVANCED_SETTINGS_DESCRIPTOR = msg({
	message: 'Search advanced settings',
	comment: 'Placeholder for the search field in the Advanced settings tab.',
});
const NEW_SETTINGS_DESCRIPTOR = msg({
	message: 'New settings',
	comment: 'Section title for recently added settings in the Advanced settings tab.',
});
const NEW_SETTINGS_DESCRIPTION_DESCRIPTOR = msg({
	message: 'New settings stay here for 30 days.',
	comment: 'Description under the New settings section title.',
});
const NO_ADVANCED_SETTINGS_FOUND_DESCRIPTOR = msg({
	message: 'No advanced settings found',
	comment: 'Empty state text in the Advanced settings tab.',
});

const EMPTY_TAGS = new Set<SettingsCategoryTag>();

export const AdvancedSettingsTab = observer(() => {
	const {i18n} = useLingui();
	const [query, setQuery] = useState('');
	const currentUserCreatedAt = Users.currentUser?.createdAt ?? null;
	const allItems = useMemo(() => getAdvancedSettingItems(), []);
	const tagLabels = useMemo(
		() =>
			ADVANCED_SETTINGS_TAG_ORDER.reduce(
				(acc, tag) => {
					acc[tag] = i18n._(ADVANCED_SETTINGS_TAG_LABELS[tag]);
					return acc;
				},
				{} as Record<SettingsCategoryTag, string>,
			),
		[i18n.locale],
	);
	const filteredItems = useMemo(() => filterAdvancedSettingItems(query, EMPTY_TAGS, allItems), [allItems, query]);
	const featuredItems = useMemo(
		() => filteredItems.filter((item) => isSettingsItemNew(item, Date.now(), currentUserCreatedAt)),
		[filteredItems, currentUserCreatedAt],
	);
	const remainingItems = useMemo(
		() => filteredItems.filter((item) => !featuredItems.includes(item)),
		[featuredItems, filteredItems],
	);
	const groups = useMemo(() => groupAdvancedItemsByCategory(remainingItems, tagLabels), [remainingItems, tagLabels]);
	const showGroupLinks = groups.length > 1;
	const handleOpenSource = useCallback((item: SearchableSettingItem) => {
		ComponentBus.dispatch('USER_SETTINGS_TAB_SELECT', {
			tab: getAdvancedSettingSourceTab(item),
			section: getAdvancedSettingSourceSection(item),
		});
	}, []);
	const hasResults = filteredItems.length > 0;
	return (
		<SettingsTabContainer data-flx="user.advanced-settings-tab.container">
			<SettingsTabContent data-flx="user.advanced-settings-tab.content">
				<div className={styles.toolbar} data-flx="user.advanced-settings-tab.toolbar">
					<SettingsSearch
						value={query}
						onChange={setQuery}
						placeholder={i18n._(SEARCH_ADVANCED_SETTINGS_DESCRIPTOR)}
						data-flx="user.advanced-settings-tab.search"
					/>
				</div>
				{!hasResults && (
					<div className={styles.emptyState} data-flx="user.advanced-settings-tab.empty-state">
						{i18n._(NO_ADVANCED_SETTINGS_FOUND_DESCRIPTOR)}
					</div>
				)}
				{featuredItems.length > 0 && (
					<section className={styles.section} data-flx="user.advanced-settings-tab.new-section">
						<div className={styles.sectionHeader} data-flx="user.advanced-settings-tab.section-header">
							<div className={styles.sectionTitleRow} data-flx="user.advanced-settings-tab.section-title-row">
								<h3 className={styles.sectionTitle} data-flx="user.advanced-settings-tab.section-title">
									{i18n._(NEW_SETTINGS_DESCRIPTOR)}
								</h3>
								<SettingsStatusBadge kind="new" data-flx="user.advanced-settings-tab.new-badge" />
							</div>
							<p className={styles.sectionDescription} data-flx="user.advanced-settings-tab.section-description">
								{i18n._(NEW_SETTINGS_DESCRIPTION_DESCRIPTOR)}
							</p>
						</div>
						<div className={styles.itemList} data-flx="user.advanced-settings-tab.item-list.new">
							{featuredItems.map((item) => (
								<AdvancedSettingRow
									key={item.id}
									item={item}
									userCreatedAt={currentUserCreatedAt}
									onOpen={handleOpenSource}
									data-flx="user.advanced-settings-tab.advanced-setting-row"
								/>
							))}
						</div>
					</section>
				)}
				{groups.map((group) => {
					const sectionId = getAdvancedSettingsCategorySectionId(group.key);
					return (
						<section
							key={group.key}
							id={sectionId}
							className={styles.section}
							data-flx="user.advanced-settings-tab.section"
						>
							<div className={styles.sectionHeader} data-flx="user.advanced-settings-tab.section-header--2">
								<div className={styles.sectionTitleRow} data-flx="user.advanced-settings-tab.section-title-row--2">
									<h3 className={styles.sectionTitle} data-flx="user.advanced-settings-tab.section-title--2">
										{group.label}
									</h3>
									{showGroupLinks ? (
										<SettingsHeadingLinkButton
											href={buildUserSettingsDeepLink('advanced_settings', sectionId)}
											data-flx="user.advanced-settings-tab.heading-link-button"
										/>
									) : null}
								</div>
							</div>
							<div className={styles.itemList} data-flx="user.advanced-settings-tab.item-list">
								{group.items.map((item) => (
									<AdvancedSettingRow
										key={item.id}
										item={item}
										userCreatedAt={currentUserCreatedAt}
										onOpen={handleOpenSource}
										data-flx="user.advanced-settings-tab.advanced-setting-row--2"
									/>
								))}
							</div>
						</section>
					);
				})}
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default AdvancedSettingsTab;
