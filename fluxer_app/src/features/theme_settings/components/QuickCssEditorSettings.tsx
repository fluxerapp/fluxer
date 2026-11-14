// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/theme_settings/components/QuickCssEditor.module.css';
import EditorPreferences, {
	EDITOR_FONT_SIZES,
	EDITOR_TAB_SIZES,
} from '@app/features/theme_settings/editor/EditorPreferences';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const FONT_SIZE_DESCRIPTOR = msg({
	message: 'Font size',
	comment: 'Setting in "Editor settings" of the quick CSS editor.',
});
const FONT_SIZE_VALUE_DESCRIPTOR = msg({
	message: '{size} px',
	comment: 'Option of the "Font size" setting in the quick CSS editor. {size} is a number of pixels.',
});
const TAB_SIZE_DESCRIPTOR = msg({
	message: 'Tab size',
	comment: 'Setting in "Editor settings" of the quick CSS editor. How many spaces wide one level of indentation is.',
});
const TAB_SIZE_VALUE_DESCRIPTOR = msg({
	message: '{count, plural, one {# space} other {# spaces}}',
	comment: 'Option of the "Tab size" setting in the quick CSS editor.',
});
const INDENT_WITH_TABS_DESCRIPTOR = msg({
	message: 'Indent with tabs',
	comment: 'Setting in "Editor settings" of the quick CSS editor.',
});
const WRAP_LONG_LINES_DESCRIPTOR = msg({
	message: 'Wrap long lines',
	comment: 'Setting in "Editor settings" of the quick CSS editor.',
});
const LINE_NUMBERS_DESCRIPTOR = msg({
	message: 'Line numbers',
	comment: 'Setting in "Editor settings" of the quick CSS editor.',
});
const SHOW_COMPLETIONS_DESCRIPTOR = msg({
	message: 'Show completions as you type',
	comment: 'Setting in "Editor settings" of the quick CSS editor. Completions are suggestions of CSS code.',
});
const LIVE_PREVIEW_DESCRIPTOR = msg({
	message: 'Live preview',
	comment: 'Setting in "Editor settings" of the quick CSS editor. When on, the app changes as you type.',
});
const LIVE_PREVIEW_HINT_DESCRIPTOR = msg({
	message: 'When this is off, changes show in the app when you select "Apply changes".',
	comment: 'Description of the "Live preview" setting in the quick CSS editor.',
});

export const QuickCssEditorSettings: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<div className={styles.settings} data-flx="theme-settings.quick-css-editor-settings.root">
			<Combobox
				label={i18n._(FONT_SIZE_DESCRIPTOR)}
				value={EditorPreferences.fontSize}
				options={EDITOR_FONT_SIZES.map((size) => ({value: size, label: i18n._(FONT_SIZE_VALUE_DESCRIPTOR, {size})}))}
				onChange={(value) => EditorPreferences.set('fontSize', value)}
				density="compact"
				data-flx="theme-settings.quick-css-editor-settings.font-size"
			/>
			<Combobox
				label={i18n._(TAB_SIZE_DESCRIPTOR)}
				value={EditorPreferences.tabSize}
				options={EDITOR_TAB_SIZES.map((size) => ({
					value: size,
					label: i18n._(TAB_SIZE_VALUE_DESCRIPTOR, {count: size}),
				}))}
				onChange={(value) => EditorPreferences.set('tabSize', value)}
				density="compact"
				data-flx="theme-settings.quick-css-editor-settings.tab-size"
			/>
			<Switch
				compact
				label={i18n._(INDENT_WITH_TABS_DESCRIPTOR)}
				value={EditorPreferences.indentWithTabs}
				onChange={(value) => EditorPreferences.set('indentWithTabs', value)}
				data-flx="theme-settings.quick-css-editor-settings.indent-with-tabs"
			/>
			<Switch
				compact
				label={i18n._(WRAP_LONG_LINES_DESCRIPTOR)}
				value={EditorPreferences.wrapLines}
				onChange={(value) => EditorPreferences.set('wrapLines', value)}
				data-flx="theme-settings.quick-css-editor-settings.wrap-lines"
			/>
			<Switch
				compact
				label={i18n._(LINE_NUMBERS_DESCRIPTOR)}
				value={EditorPreferences.lineNumbers}
				onChange={(value) => EditorPreferences.set('lineNumbers', value)}
				data-flx="theme-settings.quick-css-editor-settings.line-numbers"
			/>
			<Switch
				compact
				label={i18n._(SHOW_COMPLETIONS_DESCRIPTOR)}
				value={EditorPreferences.completeWhileTyping}
				onChange={(value) => EditorPreferences.set('completeWhileTyping', value)}
				data-flx="theme-settings.quick-css-editor-settings.complete-while-typing"
			/>
			<Switch
				compact
				label={i18n._(LIVE_PREVIEW_DESCRIPTOR)}
				description={i18n._(LIVE_PREVIEW_HINT_DESCRIPTOR)}
				value={EditorPreferences.livePreview}
				onChange={(value) => EditorPreferences.set('livePreview', value)}
				data-flx="theme-settings.quick-css-editor-settings.live-preview"
			/>
		</div>
	);
});
