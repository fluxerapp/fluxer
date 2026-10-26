// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsPageContext} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {keyboardEventMatchesCombo} from '@app/features/app/keybindings/KeybindEventUtils';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Keybind, {type KeybindCommand} from '@app/features/input/state/InputKeybind';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import AccountThemeMode from '@app/features/theme/state/AccountThemeMode';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import * as ThemeSettingsCommands from '@app/features/theme_settings/commands/ThemeSettingsCommands';
import {AssetsSection} from '@app/features/theme_settings/components/AssetsSection';
import {BaseThemeSection} from '@app/features/theme_settings/components/BaseThemeSection';
import {ColorsAndFontsSection} from '@app/features/theme_settings/components/ColorsAndFontsSection';
import {requestQuickCssEditorClose} from '@app/features/theme_settings/components/QuickCssEditorDiscard';
import {useQuickCssPanelControls} from '@app/features/theme_settings/components/QuickCssEditorPanel';
import {QuickCssFiles} from '@app/features/theme_settings/components/QuickCssFiles';
import {QuickCssReference} from '@app/features/theme_settings/components/QuickCssReference';
import styles from '@app/features/theme_settings/components/QuickCssWorkspace.module.css';
import {ThisAccountSection} from '@app/features/theme_settings/components/ThisAccountSection';
import {YourThemesSection} from '@app/features/theme_settings/components/YourThemesSection';
import type {QuickCssPanelMode} from '@app/features/theme_settings/state/QuickCssPanelLayout';
import ThemeSettingsState, {
	type QuickCssEditorFocusTarget,
	type QuickCssEditorTab,
} from '@app/features/theme_settings/state/ThemeSettingsState';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {type TabItem, Tabs, tabElementId, tabPanelElementId} from '@app/features/ui/tabs/Tabs';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {isNativeMacOS} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {
	ArrowSquareInIcon,
	ArrowSquareOutIcon,
	DotsSixVerticalIcon,
	type Icon,
	PictureInPictureIcon,
	SidebarSimpleIcon,
	XIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {reaction} from 'mobx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {createContext, useContext, useEffect, useId, useRef} from 'react';

const QUICK_CSS_EDITOR_DESCRIPTOR = msg({
	message: 'Quick CSS editor',
	comment: 'Accessible name of the quick CSS editor area, in its floating panel or its own window.',
});
const CSS_TAB_DESCRIPTOR = msg({
	message: 'CSS',
	comment: 'Tab in the quick CSS editor that shows the code editor.',
});
const COLORS_AND_FONTS_TAB_DESCRIPTOR = msg({
	message: 'Colors and fonts',
	comment: 'Tab in the quick CSS editor that shows the base theme and the color and font controls.',
});
const THEMES_TAB_DESCRIPTOR = msg({
	message: 'Themes',
	comment: 'Tab in the quick CSS editor that lists saved themes.',
});
const ASSETS_TAB_DESCRIPTOR = msg({
	message: 'Assets',
	comment: 'Tab in the quick CSS editor that lists uploaded theme assets such as images and fonts.',
});
const REFERENCE_TAB_DESCRIPTOR = msg({
	message: 'Reference',
	comment: 'Tab in the quick CSS editor that lists the CSS variables and selectors theme authors can use.',
});
const MOVE_EDITOR_DESCRIPTOR = msg({
	message: 'Move the editor. Use the arrow keys to move it.',
	comment: 'Accessible label of the drag handle in the header of the floating quick CSS editor.',
});
const DOCK_LEFT_DESCRIPTOR = msg({
	message: 'Dock to the left',
	comment: 'Button in the quick CSS editor header that attaches the editor to the left edge of the app.',
});
const DOCK_RIGHT_DESCRIPTOR = msg({
	message: 'Dock to the right',
	comment: 'Button in the quick CSS editor header that attaches the editor to the right edge of the app.',
});
const FLOAT_DESCRIPTOR = msg({
	message: 'Float',
	comment: 'Button in the quick CSS editor header that detaches a docked editor so it floats over the app again.',
});
const OPEN_IN_NEW_WINDOW_DESCRIPTOR = msg({
	message: 'Open in a new window',
	comment: 'Button in the quick CSS editor header that moves the editor into its own window.',
});
const MOVE_BACK_INTO_FLUXER_DESCRIPTOR = msg({
	message: 'Move back into Fluxer',
	comment: 'Button in the quick CSS editor window that moves the editor back into a panel inside the app.',
});

const KEYBOARD_STEP = 16;
const KEYBOARD_LARGE_STEP = 64;

const TabIdContext = createContext('quick-css');

interface HeaderButtonProps {
	label: string;
	icon: Icon;
	onClick: () => void;
	mirrored?: boolean;
	focusKey?: QuickCssEditorFocusTarget;
}

const HeaderButton: React.FC<HeaderButtonProps> = ({label, icon: IconComponent, onClick, mirrored, focusKey}) => (
	<Tooltip text={label} position="bottom" data-flx="theme-settings.quick-css-workspace.header-button.tooltip">
		<button
			type="button"
			className={styles.headerButton}
			aria-label={label}
			onClick={onClick}
			data-focus-key={focusKey}
			data-flx="theme-settings.quick-css-workspace.header-button.button"
		>
			<IconComponent
				size={remFromPx(18)}
				weight="bold"
				className={mirrored ? styles.mirroredIcon : undefined}
				data-flx="theme-settings.quick-css-workspace.header-button.icon"
			/>
		</button>
	</Tooltip>
);

const FOCUS_AFTER_MODE: Readonly<Record<QuickCssPanelMode, QuickCssEditorFocusTarget>> = {
	floating: 'dock-right',
	'dock-left': 'float',
	'dock-right': 'float',
};

const WorkspaceHeader: React.FC = observer(() => {
	const {i18n} = useLingui();
	const idPrefix = useContext(TabIdContext);
	const panel = useQuickCssPanelControls();
	const setMode = (mode: QuickCssPanelMode) => {
		if (!panel) return;
		panel.setMode(mode);
		ThemeSettingsState.requestFocus(FOCUS_AFTER_MODE[mode]);
	};
	const tabs: Array<TabItem<QuickCssEditorTab>> = [
		{key: 'css', label: i18n._(CSS_TAB_DESCRIPTOR)},
		{key: 'colors', label: i18n._(COLORS_AND_FONTS_TAB_DESCRIPTOR)},
		{key: 'themes', label: i18n._(THEMES_TAB_DESCRIPTOR)},
		{key: 'assets', label: i18n._(ASSETS_TAB_DESCRIPTOR)},
		{key: 'reference', label: i18n._(REFERENCE_TAB_DESCRIPTOR)},
	];
	const floating = panel?.mode === 'floating';
	const handleHeaderPointerDown = (event: React.PointerEvent<HTMLElement>) => {
		if (!panel || !floating) return;
		if ((event.target as Element).closest('button, [role="tab"], input, a')) return;
		panel.startMove(event);
	};
	const handleGripKeyDown = (event: React.KeyboardEvent<HTMLButtonElement>) => {
		if (!panel) return;
		const step = event.shiftKey ? KEYBOARD_LARGE_STEP : KEYBOARD_STEP;
		const deltas: Record<string, [number, number]> = {
			ArrowLeft: [-step, 0],
			ArrowRight: [step, 0],
			ArrowUp: [0, -step],
			ArrowDown: [0, step],
		};
		const delta = deltas[event.key];
		if (!delta) return;
		event.preventDefault();
		panel.moveBy(delta[0], delta[1]);
	};
	const handleClose = () => {
		requestQuickCssEditorClose(i18n, () => ThemeSettingsCommands.closeQuickCssEditor());
	};
	return (
		<div
			className={clsx(styles.header, floating && styles.headerDraggable)}
			onPointerDown={handleHeaderPointerDown}
			data-flx="theme-settings.quick-css-workspace.header"
		>
			{floating ? (
				<button
					type="button"
					className={styles.grip}
					aria-label={i18n._(MOVE_EDITOR_DESCRIPTOR)}
					onPointerDown={(event) => panel?.startMove(event)}
					onKeyDown={handleGripKeyDown}
					data-flx="theme-settings.quick-css-workspace.grip"
				>
					<DotsSixVerticalIcon
						size={remFromPx(18)}
						weight="bold"
						data-flx="theme-settings.quick-css-workspace.dots-six-vertical-icon"
					/>
				</button>
			) : null}
			<div className={styles.tabs} data-flx="theme-settings.quick-css-workspace.tabs">
				<Tabs
					tabs={tabs}
					activeTab={ThemeSettingsState.editorTab}
					onTabChange={ThemeSettingsState.setEditorTab}
					idPrefix={idPrefix}
					className={styles.tabList}
					data-flx="theme-settings.quick-css-workspace.tabs.set-editor-tab"
				/>
			</div>
			<div className={styles.headerActions} data-flx="theme-settings.quick-css-workspace.header-actions">
				{panel ? (
					<>
						{panel.canDock && panel.mode !== 'dock-left' ? (
							<HeaderButton
								label={i18n._(DOCK_LEFT_DESCRIPTOR)}
								icon={SidebarSimpleIcon}
								focusKey="dock-left"
								onClick={() => setMode('dock-left')}
								data-flx="theme-settings.quick-css-workspace.header-button.dock-left"
							/>
						) : null}
						{panel.canDock && panel.mode !== 'dock-right' ? (
							<HeaderButton
								label={i18n._(DOCK_RIGHT_DESCRIPTOR)}
								icon={SidebarSimpleIcon}
								mirrored
								focusKey="dock-right"
								onClick={() => setMode('dock-right')}
								data-flx="theme-settings.quick-css-workspace.header-button.dock-right"
							/>
						) : null}
						{panel.mode !== 'floating' ? (
							<HeaderButton
								label={i18n._(FLOAT_DESCRIPTOR)}
								icon={PictureInPictureIcon}
								focusKey="float"
								onClick={() => setMode('floating')}
								data-flx="theme-settings.quick-css-workspace.header-button.float"
							/>
						) : null}
						{ThemeSettingsCommands.canPopOutQuickCssEditor() ? (
							<HeaderButton
								label={i18n._(OPEN_IN_NEW_WINDOW_DESCRIPTOR)}
								icon={ArrowSquareOutIcon}
								onClick={() => ThemeSettingsCommands.moveQuickCssEditor('window')}
								data-flx="theme-settings.quick-css-workspace.header-button.open-in-new-window"
							/>
						) : null}
						<HeaderButton
							label={i18n._(CLOSE_DESCRIPTOR)}
							icon={XIcon}
							onClick={handleClose}
							data-flx="theme-settings.quick-css-workspace.header-button.close"
						/>
					</>
				) : (
					<HeaderButton
						label={i18n._(MOVE_BACK_INTO_FLUXER_DESCRIPTOR)}
						icon={ArrowSquareInIcon}
						onClick={() => ThemeSettingsCommands.moveQuickCssEditor('panel')}
						data-flx="theme-settings.quick-css-workspace.header-button.move-back"
					/>
				)}
			</div>
		</div>
	);
});

const SettingsTabBody: React.FC<{tab: QuickCssEditorTab; children: React.ReactNode}> = ({tab, children}) => {
	const idPrefix = useContext(TabIdContext);
	return (
		<div
			role="tabpanel"
			id={tabPanelElementId(idPrefix, tab)}
			aria-labelledby={tabElementId(idPrefix, tab)}
			className={styles.settingsBody}
			data-flx="theme-settings.quick-css-workspace.settings-body"
		>
			<SettingsPageContext.Provider value={false}>
				<div className={styles.settingsContent} data-flx="theme-settings.quick-css-workspace.settings-content">
					{children}
				</div>
			</SettingsPageContext.Provider>
		</div>
	);
};

function focusRequestedTarget(root: HTMLElement, target: QuickCssEditorFocusTarget): boolean {
	if (target === 'editor') return false;
	const selectedTab = root.querySelector<HTMLElement>('[role="tab"][aria-selected="true"]');
	const element =
		target === 'tab' ? selectedTab : (root.querySelector<HTMLElement>(`[data-focus-key="${target}"]`) ?? selectedTab);
	element?.focus();
	return true;
}

export const QuickCssWorkspace: React.FC = observer(() => {
	const {i18n} = useLingui();
	const idPrefix = useId();
	const rootRef = useRef<HTMLDivElement | null>(null);
	const tab = ThemeSettingsState.editorTab;
	useEffect(() => {
		void ThemeLibrary.init();
	}, []);
	useEffect(
		() =>
			reaction(
				() => ThemeSettingsState.pendingFocus,
				(target) => {
					const root = rootRef.current;
					if (!root || !target || !focusRequestedTarget(root, target)) return;
					ThemeSettingsState.requestFocus(null);
				},
				{fireImmediately: true, scheduler: (run) => requestAnimationFrame(run)},
			),
		[],
	);
	useEffect(() => {
		const root = rootRef.current;
		if (!root) return;
		const ownerDocument = root.ownerDocument;
		ThemeSettingsCommands.setQuickCssEditorFocusHandle({
			focus: () => {
				const editor = root.querySelector<HTMLElement>('[role="tabpanel"]:not([hidden]) .cm-content');
				const fallback = root.querySelector<HTMLElement>('[role="tab"][aria-selected="true"]');
				(editor ?? fallback)?.focus();
			},
			hasFocus: () => ownerDocument.hasFocus() && root.contains(ownerDocument.activeElement),
		});
		const matches = (action: KeybindCommand, event: KeyboardEvent): boolean =>
			Keybind.getActiveCombosForAction(action).some((combo) =>
				keyboardEventMatchesCombo(combo, event, {isMacOS: isNativeMacOS()}),
			);
		const handleKeyDown = (event: KeyboardEvent): void => {
			if (matches('system_open_theme_studio_popout', event)) {
				event.preventDefault();
				event.stopPropagation();
				ThemeSettingsCommands.toggleQuickCssEditorFocus();
				return;
			}
			if (matches('system_toggle_custom_css', event)) {
				event.preventDefault();
				event.stopPropagation();
				ThemeSettingsCommands.toggleCustomCss();
			}
		};
		ownerDocument.addEventListener('keydown', handleKeyDown, true);
		return () => {
			ownerDocument.removeEventListener('keydown', handleKeyDown, true);
			ThemeSettingsCommands.setQuickCssEditorFocusHandle(null);
		};
	}, []);
	return (
		<div
			ref={rootRef}
			role="region"
			aria-label={i18n._(QUICK_CSS_EDITOR_DESCRIPTOR)}
			className={styles.root}
			data-flx="theme-settings.quick-css-workspace.root"
		>
			<TabIdContext.Provider value={idPrefix}>
				<FocusRingScope containerRef={rootRef} data-flx="theme-settings.quick-css-workspace.focus-ring-scope">
					<WorkspaceHeader data-flx="theme-settings.quick-css-workspace.workspace-header" />
					<div
						role="tabpanel"
						id={tabPanelElementId(idPrefix, 'css')}
						aria-labelledby={tabElementId(idPrefix, 'css')}
						hidden={tab !== 'css'}
						className={styles.cssPanel}
						data-flx="theme-settings.quick-css-workspace.css-panel"
					>
						<QuickCssFiles data-flx="theme-settings.quick-css-workspace.quick-css-files" />
					</div>
					{tab === 'colors' ? (
						<SettingsTabBody tab="colors" data-flx="theme-settings.quick-css-workspace.colors">
							<BaseThemeSection data-flx="theme-settings.quick-css-workspace.base-theme-section" />
							<ColorsAndFontsSection data-flx="theme-settings.quick-css-workspace.colors-and-fonts-section" />
						</SettingsTabBody>
					) : null}
					{tab === 'themes' ? (
						<SettingsTabBody tab="themes" data-flx="theme-settings.quick-css-workspace.themes">
							{AccountThemeMode.canUseDesktopTheme ? (
								<ThisAccountSection data-flx="theme-settings.quick-css-workspace.this-account-section" />
							) : null}
							<YourThemesSection data-flx="theme-settings.quick-css-workspace.your-themes-section" />
						</SettingsTabBody>
					) : null}
					{tab === 'reference' ? (
						<SettingsTabBody tab="reference" data-flx="theme-settings.quick-css-workspace.reference">
							<QuickCssReference data-flx="theme-settings.quick-css-workspace.quick-css-reference" />
						</SettingsTabBody>
					) : null}
					{tab === 'assets' ? (
						<SettingsTabBody tab="assets" data-flx="theme-settings.quick-css-workspace.assets">
							<AssetsSection data-flx="theme-settings.quick-css-workspace.assets-section" />
						</SettingsTabBody>
					) : null}
				</FocusRingScope>
			</TabIdContext.Provider>
		</div>
	);
});
