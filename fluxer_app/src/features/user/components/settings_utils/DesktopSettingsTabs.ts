// SPDX-License-Identifier: AGPL-3.0-or-later

import {ThemesTab} from '@app/features/theme_settings/ThemesTab';
import {AccessibilityTab} from '@app/features/user/components/modals/tabs/AccessibilityTab';
import AccountSecurityTab from '@app/features/user/components/modals/tabs/AccountSecurityTab';
import AdvancedSettingsTab from '@app/features/user/components/modals/tabs/AdvancedSettingsTab';
import {AppearanceTab} from '@app/features/user/components/modals/tabs/AppearanceTab';
import ApplicationsTab from '@app/features/user/components/modals/tabs/applications_tab';
import ChatSettingsTab from '@app/features/user/components/modals/tabs/ChatSettingsTab';
import ComponentGalleryTab from '@app/features/user/components/modals/tabs/component_gallery_tab';
import DesktopSettingsTab from '@app/features/user/components/modals/tabs/DesktopSettingsTab';
import EmbedDebuggerTab from '@app/features/user/components/modals/tabs/EmbedDebuggerTab';
import GiftInventoryTab from '@app/features/user/components/modals/tabs/GiftInventoryTab';
import KeybindsTab from '@app/features/user/components/modals/tabs/KeybindsTab';
import LanguageTab from '@app/features/user/components/modals/tabs/LanguageTab';
import LinkedAccountsTab from '@app/features/user/components/modals/tabs/LinkedAccountsTab';
import MyProfileTab from '@app/features/user/components/modals/tabs/MyProfileTab';
import NotificationsTab from '@app/features/user/components/modals/tabs/NotificationsTab';
import OperatorPassTab from '@app/features/user/components/modals/tabs/OperatorPassTab';
import PaymentsTab from '@app/features/user/components/modals/tabs/PaymentsTab';
import PlutoniumTab from '@app/features/user/components/modals/tabs/PlutoniumTab';
import PrivacySafetyTab from '@app/features/user/components/modals/tabs/PrivacySafetyTab';
import SubscriptionsTab from '@app/features/user/components/modals/tabs/SubscriptionsTab';
import VoiceVideoTab from '@app/features/user/components/modals/tabs/VoiceVideoTab';
import type {UserSettingsTabType} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import type React from 'react';

const DESKTOP_TAB_COMPONENTS: Partial<Record<UserSettingsTabType, React.ComponentType<Record<string, unknown>>>> = {
	my_profile: MyProfileTab,
	account_security: AccountSecurityTab,
	plutonium: PlutoniumTab,
	subscriptions: SubscriptionsTab,
	operator_pass: OperatorPassTab,
	gift_inventory: GiftInventoryTab,
	payments: PaymentsTab,
	privacy_safety: PrivacySafetyTab,
	authorized_apps: AccountSecurityTab,
	blocked_users: AccountSecurityTab,
	devices: AccountSecurityTab,
	linked_accounts: LinkedAccountsTab,
	appearance: AppearanceTab,
	themes: ThemesTab,
	accessibility: AccessibilityTab,
	chat_settings: ChatSettingsTab,
	voice_video: VoiceVideoTab,
	keybinds: KeybindsTab,
	notifications: NotificationsTab,
	language: LanguageTab,
	desktop_settings: DesktopSettingsTab,
	advanced_settings: AdvancedSettingsTab,
	embed_debugger: EmbedDebuggerTab,
	applications: ApplicationsTab,
	component_gallery: ComponentGalleryTab,
};
export const getSettingsTabComponent = (
	tabType: UserSettingsTabType,
): React.ComponentType<Record<string, unknown>> | null => {
	return DESKTOP_TAB_COMPONENTS[tabType] ?? null;
};
