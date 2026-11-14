// SPDX-License-Identifier: AGPL-3.0-or-later

import {createNamedLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {excludeUserStylesheets} from '@app/features/theme/hooks/useCustomThemeStyle';
import ThemeLibrary, {getThemeLabel} from '@app/features/theme/state/ThemeLibrary';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import {QUICK_CSS_EDITOR_WINDOW_KEY} from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import {requestQuickCssEditorClose} from '@app/features/theme_settings/components/QuickCssEditorDiscard';
import {QuickCssEditorPanel} from '@app/features/theme_settings/components/QuickCssEditorPanel';
import ThemeSettingsState, {QUICK_CSS_EDITOR_FILE} from '@app/features/theme_settings/state/ThemeSettingsState';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {PopoutWindow, type PopoutWindowCloseReason} from '@app/features/ui/popout_window/PopoutWindow';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {QUICK_CSS_SECTION_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/ThemesSections';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

const logger = new Logger('QuickCssEditorHost');

const POPUP_BLOCKED_DESCRIPTOR = msg({
	message: 'Your browser blocked the editor window. Allow pop-ups for this site and try again.',
	comment: 'Toast on the web app when the browser blocks the quick CSS editor window from opening.',
});

const EDITOR_WINDOW_WIDTH = 1100;
const EDITOR_WINDOW_HEIGHT = 760;
const EDITOR_WINDOW_NAME = isDesktop()
	? QUICK_CSS_EDITOR_WINDOW_KEY
	: `${QUICK_CSS_EDITOR_WINDOW_KEY}_${Math.random().toString(36).slice(2, 10)}`;

const QuickCssWorkspace = createNamedLoadableComponent<Record<string, never>>({
	displayName: 'QuickCssWorkspace',
	load: async () => (await import('@app/features/theme_settings/components/QuickCssWorkspace')).QuickCssWorkspace,
});

const QuickCssEditorWindow: React.FC<{generation: number}> = observer(({generation}) => {
	const {i18n} = useLingui();
	const [isAlwaysOnTop, setIsAlwaysOnTop] = useState(isDesktop());
	const theme =
		ThemeSettingsState.editorFile === QUICK_CSS_EDITOR_FILE
			? null
			: ThemeLibrary.themes.find((candidate) => candidate.id === ThemeSettingsState.editorFile);
	const title = theme ? getThemeLabel(theme) : i18n._(QUICK_CSS_SECTION_DESCRIPTOR);
	const handleWindowOpened = useCallback((childWindow: Window) => {
		ThemeSettingsState.setEditorWindow(childWindow);
	}, []);
	const handleClosed = useCallback(
		(windowGeneration: number, reason: PopoutWindowCloseReason) => {
			if (reason === 'closed') {
				ThemeSettingsCommands.closeQuickCssEditor(windowGeneration);
				return;
			}
			if (reason === 'blocked') {
				ToastCommands.error(i18n._(POPUP_BLOCKED_DESCRIPTOR));
			}
			ThemeSettingsState.moveEditor('panel', windowGeneration);
		},
		[i18n],
	);
	const handleCloseRequest = useCallback(
		(close: () => void) => {
			requestQuickCssEditorClose(i18n, close);
		},
		[i18n],
	);
	const electronApi = getElectronAPI();
	const handleToggleAlwaysOnTop = useCallback(() => {
		const next = !isAlwaysOnTop;
		void getElectronAPI()
			?.popoutSetAlwaysOnTop?.(QUICK_CSS_EDITOR_WINDOW_KEY, next)
			.then((changed) => {
				if (changed) setIsAlwaysOnTop(next);
			})
			.catch((error) => logger.warn('Failed to toggle the quick CSS editor always-on-top', error));
	}, [isAlwaysOnTop]);
	return (
		<PopoutWindow
			windowKey={EDITOR_WINDOW_NAME}
			windowGeneration={generation}
			title={title}
			width={EDITOR_WINDOW_WIDTH}
			height={EDITOR_WINDOW_HEIGHT}
			isAlwaysOnTop={isAlwaysOnTop}
			onToggleAlwaysOnTop={electronApi?.popoutSetAlwaysOnTop ? handleToggleAlwaysOnTop : undefined}
			onClosed={handleClosed}
			onCloseRequest={handleCloseRequest}
			onWindowOpened={handleWindowOpened}
			stylesheetExclude={excludeUserStylesheets}
			data-flx="theme-settings.quick-css-editor-host.quick-css-editor-window.popout-window"
		>
			<QuickCssWorkspace data-flx="theme-settings.quick-css-editor-host.quick-css-editor-window.quick-css-workspace" />
		</PopoutWindow>
	);
});

export const QuickCssEditorHost: React.FC = observer(() => {
	const placement = ThemeSettingsState.editorPlacement;
	const generation = ThemeSettingsState.editorGeneration;
	useEffect(() => {
		const handlePageHide = (): void => {
			ThemeSettingsCommands.flushQuickCssWrites();
			ThemeSettingsState.editorWindow?.close();
		};
		const handleVisibilityChange = (): void => {
			if (document.visibilityState === 'hidden') ThemeSettingsCommands.flushQuickCssWrites();
		};
		window.addEventListener('pagehide', handlePageHide);
		document.addEventListener('visibilitychange', handleVisibilityChange);
		return () => {
			window.removeEventListener('pagehide', handlePageHide);
			document.removeEventListener('visibilitychange', handleVisibilityChange);
		};
	}, []);
	if (placement === 'panel') {
		return (
			<QuickCssEditorPanel key={generation} data-flx="theme-settings.quick-css-editor-host.quick-css-editor-panel">
				<QuickCssWorkspace data-flx="theme-settings.quick-css-editor-host.quick-css-workspace" />
			</QuickCssEditorPanel>
		);
	}
	if (placement === 'window') {
		return (
			<QuickCssEditorWindow
				key={generation}
				generation={generation}
				data-flx="theme-settings.quick-css-editor-host.quick-css-editor-window"
			/>
		);
	}
	return null;
});
