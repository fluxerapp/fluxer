// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SCREEN_READER_DESCRIPTOR, TEXT_TO_SPEECH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {
	AccessibilityScreenReaderTabContent,
	AccessibilityTtsTabContent,
} from '@app/features/user/components/modals/tabs/AccessibilityTab';
import {AnimationTabContent} from '@app/features/user/components/modals/tabs/accessibility_tab/AnimationTab';
import {KeyboardTabContent} from '@app/features/user/components/modals/tabs/accessibility_tab/KeyboardTab';
import {MotionTabContent} from '@app/features/user/components/modals/tabs/accessibility_tab/MotionTab';
import {VisualTabContent} from '@app/features/user/components/modals/tabs/accessibility_tab/VisualTab';
import styles from '@app/features/user/components/modals/tabs/TabInline.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const VISUAL_DESCRIPTOR = msg({
	message: 'Visual',
	comment: 'Short label in the inline. Keep it concise.',
});
const KEYBOARD_DESCRIPTOR = msg({
	message: 'Keyboard',
	comment: 'Short label in the inline. Keep it concise.',
});
const ANIMATION_DESCRIPTOR = msg({
	message: 'Animation',
	comment: 'Short label in the inline. Keep it concise.',
});
const MOTION_DESCRIPTOR = msg({
	message: 'Motion',
	comment: 'Short label in the inline. Keep it concise.',
});
export const AccessibilityInlineContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<div className={styles.container} data-flx="user.accessibility-tab.inline.accessibility-inline-content.container">
			<SettingsSection
				id="visual"
				title={i18n._(VISUAL_DESCRIPTOR)}
				data-flx="user.accessibility-tab.inline.accessibility-inline-content.visual"
			>
				<VisualTabContent data-flx="user.accessibility-tab.inline.accessibility-inline-content.visual-tab-content" />
			</SettingsSection>
			<SettingsSection
				id="screen-reader"
				title={i18n._(SCREEN_READER_DESCRIPTOR)}
				data-flx="user.accessibility-tab.inline.accessibility-inline-content.screen-reader"
			>
				<AccessibilityScreenReaderTabContent data-flx="user.accessibility-tab.inline.accessibility-inline-content.accessibility-screen-reader-tab-content" />
			</SettingsSection>
			<SettingsSection
				id="tts"
				title={i18n._(TEXT_TO_SPEECH_DESCRIPTOR)}
				data-flx="user.accessibility-tab.inline.accessibility-inline-content.tts"
			>
				<AccessibilityTtsTabContent data-flx="user.accessibility-tab.inline.accessibility-inline-content.accessibility-tts-tab-content" />
			</SettingsSection>
			<SettingsSection
				id="keyboard"
				title={i18n._(KEYBOARD_DESCRIPTOR)}
				data-flx="user.accessibility-tab.inline.accessibility-inline-content.keyboard"
			>
				<KeyboardTabContent data-flx="user.accessibility-tab.inline.accessibility-inline-content.keyboard-tab-content" />
			</SettingsSection>
			<SettingsSection
				id="animation"
				title={i18n._(ANIMATION_DESCRIPTOR)}
				data-flx="user.accessibility-tab.inline.accessibility-inline-content.animation"
			>
				<AnimationTabContent data-flx="user.accessibility-tab.inline.accessibility-inline-content.animation-tab-content" />
			</SettingsSection>
			<SettingsSection
				id="motion"
				title={i18n._(MOTION_DESCRIPTOR)}
				data-flx="user.accessibility-tab.inline.accessibility-inline-content.motion"
			>
				<MotionTabContent data-flx="user.accessibility-tab.inline.accessibility-inline-content.motion-tab-content" />
			</SettingsSection>
		</div>
	);
});
