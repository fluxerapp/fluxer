// SPDX-License-Identifier: AGPL-3.0-or-later

import {APP_PROTOCOL_PREFIX} from '@app/features/ui/utils/AppProtocol';
import {isAdvancedSettingsCategorySectionId} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedSettingsCategories';
import {
	ACCOUNT_SETTINGS_TAB,
	getAccountSectionForLegacySection,
	getAccountSectionForNestedTab,
	isAccountNestedSettingsTab,
} from '@app/features/user/components/settings_utils/SettingsNavigationGroups';
import {
	getAllSectionDefinitions,
	isSectionIdValid,
	type UserSettingsTabType,
} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';

export const USER_SETTINGS_DEEP_LINK_PATH = '/settings/user';

const USER_SETTINGS_TAB_TYPES = new Set<UserSettingsTabType>([
	'my_profile',
	'account_security',
	'plutonium',
	'gift_inventory',
	'privacy_safety',
	'authorized_apps',
	'blocked_users',
	'devices',
	'appearance',
	'accessibility',
	'chat_settings',
	'voice_video',
	'notifications',
	'desktop_settings',
	'advanced_settings',
	'client_developer_settings',
	'embed_debugger',
	'applications',
	'component_gallery',
	'language',
	'keybinds',
	'linked_accounts',
]);
const SAFE_SETTINGS_PARAM_REGEX = /^[A-Za-z0-9_-]+$/;

export interface UserSettingsDeepLinkTarget {
	tab: UserSettingsTabType;
	section?: string;
}

export function isUserSettingsTabType(value: string | null | undefined): value is UserSettingsTabType {
	return !!value && SAFE_SETTINGS_PARAM_REGEX.test(value) && USER_SETTINGS_TAB_TYPES.has(value as UserSettingsTabType);
}

export function isUserSettingsSectionTarget(tabType: UserSettingsTabType, sectionId: string): boolean {
	if (!SAFE_SETTINGS_PARAM_REGEX.test(sectionId)) return false;
	if (isSectionIdValid(sectionId, tabType)) return true;
	return tabType === 'advanced_settings' && isAdvancedSettingsCategorySectionId(sectionId);
}

export function getUserSettingsSectionDeepLinkTarget(
	sectionId: string,
	tabType?: UserSettingsTabType,
): UserSettingsDeepLinkTarget | null {
	if (tabType) {
		return isUserSettingsSectionTarget(tabType, sectionId) ? {tab: tabType, section: sectionId} : null;
	}
	const matchingSections = getAllSectionDefinitions().filter((section) => section.id === sectionId);
	if (matchingSections.length !== 1) return null;
	const section = matchingSections[0];
	if (!section) return null;
	return {tab: section.tabType, section: section.id};
}

export function buildUserSettingsDeepLink(tabType: UserSettingsTabType, sectionId?: string): string {
	const accountSection = getAccountSectionForNestedTab(tabType) ?? getAccountSectionForLegacySection(sectionId);
	const shouldUseAccountTab = accountSection !== null || isAccountNestedSettingsTab(tabType);
	const resolvedTabType = shouldUseAccountTab ? ACCOUNT_SETTINGS_TAB : tabType;
	const resolvedSectionId = accountSection ?? sectionId;
	const params = new URLSearchParams({tab: resolvedTabType});
	if (resolvedSectionId) {
		params.set('section', resolvedSectionId);
	}
	return `${APP_PROTOCOL_PREFIX}settings/user?${params.toString()}`;
}

export function parseUserSettingsDeepLinkPath(path: string): UserSettingsDeepLinkTarget | null {
	try {
		const parsed = new URL(path, 'https://fluxer.app');
		const normalizedPath = parsed.pathname.replace(/\/+$/, '') || '/';
		if (normalizedPath !== USER_SETTINGS_DEEP_LINK_PATH) return null;
		const tab = parsed.searchParams.get('tab');
		if (!isUserSettingsTabType(tab)) return null;
		const sectionParam = parsed.searchParams.get('section') ?? undefined;
		const rawSection = sectionParam && sectionParam.trim().length > 0 ? sectionParam : undefined;
		const accountSection = getAccountSectionForNestedTab(tab) ?? getAccountSectionForLegacySection(rawSection);
		const resolvedTab = accountSection ? ACCOUNT_SETTINGS_TAB : tab;
		const section = accountSection ?? rawSection;
		if (section && !isUserSettingsSectionTarget(resolvedTab, section)) return null;
		return {tab: resolvedTab, section};
	} catch {
		return null;
	}
}
