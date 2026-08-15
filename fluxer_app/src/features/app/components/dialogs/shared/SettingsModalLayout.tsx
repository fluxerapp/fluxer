// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/app/components/dialogs/shared/SettingsModalLayout.module.css';
import {SETTINGS_SECTIONS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {
	handleSettingsTreeKeyDown,
	type SettingsModalSidebarItemProps,
	type SettingsTreeApi,
	syncSidebarTabStops,
	useSettingsModalSidebarItemLogic,
} from '@app/features/user/utils/SettingsModalLayoutUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretRightIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useContext, useEffect, useId, useLayoutEffect, useMemo, useRef, useState} from 'react';

const SIDEBAR_TREE_INSTRUCTIONS_DESCRIPTOR = msg({
	message:
		'Use the Up and Down arrow keys to move between items and Space or Enter to open one. Where a tab has sections, the Right arrow expands it and the Left arrow collapses it.',
	comment:
		'Visually hidden help text announced to screen readers describing how to navigate the settings sidebar tree.',
});

const NOOP_SETTINGS_TREE_API: SettingsTreeApi = {
	isExpanded: () => false,
	expand: () => {},
	collapse: () => {},
	toggle: () => {},
};
const SettingsTreeContext = React.createContext<SettingsTreeApi>(NOOP_SETTINGS_TREE_API);
export const SettingsTreeProvider = SettingsTreeContext.Provider;
export function useSettingsTree(): SettingsTreeApi {
	return useContext(SettingsTreeContext);
}

interface SettingsModalContextValue {
	fullscreen: boolean;
}

const SettingsModalContext = React.createContext<SettingsModalContextValue>({
	fullscreen: false,
});
export const SettingsModalContainer: React.FC<{children: React.ReactNode; fullscreen?: boolean}> = observer(
	({children, fullscreen = false}) => {
		const contextValue = useMemo(() => ({fullscreen}), [fullscreen]);
		return (
			<SettingsModalContext.Provider value={contextValue}>
				<div
					className={clsx(styles.container, {[styles.containerFullscreen]: fullscreen})}
					data-flx="app.settings-modal-layout.settings-modal-container.container"
				>
					{children}
				</div>
			</SettingsModalContext.Provider>
		);
	},
);
export const SettingsModalDesktopSidebar: React.FC<{children: React.ReactNode}> = observer(({children}) => {
	return (
		<div
			className={styles.desktopSidebar}
			data-flx="app.settings-modal-layout.settings-modal-desktop-sidebar.desktop-sidebar"
		>
			<div
				className={styles.desktopSidebarInner}
				data-flx="app.settings-modal-layout.settings-modal-desktop-sidebar.desktop-sidebar-inner"
			>
				{children}
			</div>
		</div>
	);
});

interface SettingsModalDesktopContentProps {
	children: React.ReactNode;
	tabpanelId?: string;
	labelledBy?: string;
}

const SettingsModalDesktopContentComponent = React.forwardRef<HTMLDivElement, SettingsModalDesktopContentProps>(
	({children, tabpanelId, labelledBy}, ref) => {
		return (
			<div
				ref={ref}
				className={styles.desktopContent}
				role="region"
				id={tabpanelId}
				aria-labelledby={labelledBy}
				tabIndex={-1}
				data-flx="app.settings-modal-layout.settings-modal-desktop-content-component.desktop-content"
			>
				<div
					className={styles.desktopContentPad}
					data-flx="app.settings-modal-layout.settings-modal-desktop-content-component.desktop-content-pad"
				>
					<div
						className={styles.desktopContentCard}
						data-flx="app.settings-modal-layout.settings-modal-desktop-content-component.desktop-content-card"
					>
						{children}
					</div>
				</div>
			</div>
		);
	},
);
export const SettingsModalDesktopContent = observer(SettingsModalDesktopContentComponent);

interface SettingsModalDesktopScrollProps {
	children: React.ReactNode;
	scrollKey?: string;
	scrollerRef?: React.Ref<HTMLElement | null>;
}

export const SettingsModalDesktopScroll: React.FC<SettingsModalDesktopScrollProps> = observer(
	({children, scrollKey, scrollerRef}) => {
		const internalRef = useRef<ScrollerHandle | null>(null);
		useEffect(() => {
			if (!scrollerRef) {
				return;
			}
			const node = internalRef.current?.getScrollerNode() ?? null;
			if (typeof scrollerRef === 'function') {
				scrollerRef(node);
			} else {
				(scrollerRef as React.MutableRefObject<HTMLElement | null>).current = node;
			}
		});
		return (
			<Scroller
				ref={internalRef}
				className={styles.desktopScroll}
				key={scrollKey ?? 'settings-modal-desktop-scroll'}
				data-settings-scroll-container
				data-flx="app.settings-modal-layout.settings-modal-desktop-scroll.desktop-scroll"
			>
				<div
					className={styles.desktopScrollSpacerTop}
					data-flx="app.settings-modal-layout.settings-modal-desktop-scroll.desktop-scroll-spacer-top"
				/>
				<div
					className={styles.desktopScrollInner}
					data-flx="app.settings-modal-layout.settings-modal-desktop-scroll.desktop-scroll-inner"
				>
					{children}
				</div>
				<div
					className={styles.desktopScrollSpacerBottom}
					data-flx="app.settings-modal-layout.settings-modal-desktop-scroll.desktop-scroll-spacer-bottom"
				/>
			</Scroller>
		);
	},
);

interface SidebarCategoryContextValue {
	setTitleId: (id: string | null) => void;
}

const SidebarCategoryContext = React.createContext<SidebarCategoryContextValue | null>(null);

interface SettingsModalSidebarNavProps {
	children: React.ReactNode;
	header?: React.ReactNode;
	footer?: React.ReactNode;
}

export const SettingsModalSidebarNav: React.FC<SettingsModalSidebarNavProps> = observer(
	({children, header, footer}) => {
		const {i18n} = useLingui();
		const tree = useSettingsTree();
		const sidebarListRef = useRef<HTMLDivElement>(null);
		const instructionsId = useId();
		useLayoutEffect(() => {
			syncSidebarTabStops(sidebarListRef.current);
		});
		const handleTreeKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLDivElement>) => {
				handleSettingsTreeKeyDown(event, tree);
			},
			[tree],
		);
		return (
			<>
				{header && (
					<div
						className={styles.sidebarHeader}
						data-flx="app.settings-modal-layout.settings-modal-sidebar-nav.sidebar-header"
					>
						{header}
					</div>
				)}
				<nav
					aria-label={i18n._(SETTINGS_SECTIONS_DESCRIPTOR)}
					className={styles.sidebarNavWrapper}
					data-flx="app.settings-modal-layout.settings-modal-sidebar-nav.sidebar-nav-wrapper"
				>
					<Scroller
						className={styles.sidebarNav}
						key="settings-modal-sidebar-nav"
						data-flx="app.settings-modal-layout.settings-modal-sidebar-nav.sidebar-nav"
					>
						<div
							className={styles.sidebarNavContent}
							data-flx="app.settings-modal-layout.settings-modal-sidebar-nav.sidebar-nav-content"
						>
							<p
								id={instructionsId}
								className={styles.srOnly}
								data-flx="app.settings-modal-layout.settings-modal-sidebar-nav.sidebar-nav-instructions"
							>
								{i18n._(SIDEBAR_TREE_INSTRUCTIONS_DESCRIPTOR)}
							</p>
							<div
								ref={sidebarListRef}
								className={styles.sidebarNavList}
								data-settings-sidebar-list
								role="tree"
								aria-orientation="vertical"
								aria-label={i18n._(SETTINGS_SECTIONS_DESCRIPTOR)}
								aria-describedby={instructionsId}
								onKeyDownCapture={handleTreeKeyDown}
								data-flx="app.settings-modal-layout.settings-modal-sidebar-nav.sidebar-nav-list.sidebar-nav-key-down"
							>
								{children}
							</div>
							{footer && (
								<div
									className={styles.sidebarNavFooter}
									data-flx="app.settings-modal-layout.settings-modal-sidebar-nav.sidebar-nav-footer"
								>
									{footer}
								</div>
							)}
						</div>
					</Scroller>
				</nav>
			</>
		);
	},
);
export const SettingsModalSidebarCategory: React.FC<{children: React.ReactNode}> = observer(({children}) => {
	const [titleId, setTitleId] = useState<string | null>(null);
	const contextValue = useMemo<SidebarCategoryContextValue>(() => ({setTitleId}), []);
	return (
		<SidebarCategoryContext.Provider value={contextValue}>
			<section
				className={styles.sidebarCategory}
				aria-labelledby={titleId ?? undefined}
				data-flx="app.settings-modal-layout.settings-modal-sidebar-category.sidebar-category"
			>
				{children}
			</section>
		</SidebarCategoryContext.Provider>
	);
});
export const SettingsModalSidebarCategoryTitle: React.FC<{children: React.ReactNode}> = observer(({children}) => {
	const context = useContext(SidebarCategoryContext);
	const titleId = useId();
	useEffect(() => {
		context?.setTitleId(titleId);
		return () => context?.setTitleId(null);
	}, [context, titleId]);
	return (
		<h2
			id={titleId}
			className={styles.sidebarCategoryTitle}
			data-flx="app.settings-modal-layout.settings-modal-sidebar-category-title.sidebar-category-title"
		>
			{children}
		</h2>
	);
});
export const SettingsModalSidebarItem: React.FC<SettingsModalSidebarItemProps> = observer(
	({
		label,
		icon: IconComponent,
		iconWeight = 'fill',
		selected,
		danger,
		autoSelectOnKeyboardNavigation = true,
		onClick,
		onRequestContentFocus,
		id,
		controlsId,
		expandableId,
		sectionsGroupId,
		toggleOnSelectedClick = true,
	}) => {
		const {tabIndex, buttonRef} = useSettingsModalSidebarItemLogic({selected});
		const focusContentAfterActivationRef = useRef(false);
		const tree = useSettingsTree();
		const reducedMotion = Accessibility.useReducedMotion;
		const isTabRole = id != null && controlsId != null;
		const expandable = isTabRole && expandableId != null;
		const expanded = expandable ? tree.isExpanded(expandableId) : false;
		const handleClick = useCallback(() => {
			const shouldFocusContent = focusContentAfterActivationRef.current;
			focusContentAfterActivationRef.current = false;
			buttonRef.current?.focus();
			if (expandable && selected && toggleOnSelectedClick) {
				tree.toggle(expandableId);
				return;
			}
			onClick?.();
			if (shouldFocusContent) {
				window.requestAnimationFrame(() => onRequestContentFocus?.());
			}
		}, [buttonRef, expandable, expandableId, onClick, onRequestContentFocus, selected, toggleOnSelectedClick, tree]);
		const handleKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLButtonElement>) => {
				const isActivationKey = event.key === 'Enter' || event.key === ' ' || event.key === 'Spacebar';
				if (isActivationKey && !(expandable && selected)) {
					focusContentAfterActivationRef.current = true;
				}
			},
			[expandable, selected],
		);
		const handleBlur = useCallback(() => {
			focusContentAfterActivationRef.current = false;
		}, []);
		const sharedProps = {
			ref: buttonRef,
			id,
			type: 'button' as const,
			className: clsx(styles.sidebarItem, {
				[styles.sidebarItemSelected]: selected,
				[styles.sidebarItemDanger]: danger,
			}),
			onClick: handleClick,
			onKeyDown: handleKeyDown,
			onBlur: handleBlur,
			tabIndex,
			'data-settings-sidebar-item': 'true',
			'data-reduced-motion': reducedMotion ? 'true' : undefined,
		};
		const content = (
			<>
				<IconComponent
					className={styles.sidebarItemIcon}
					size={remFromPx(20)}
					weight={iconWeight}
					data-flx="app.settings-modal-layout.settings-modal-sidebar-item.sidebar-item-icon"
				/>
				<span
					className={styles.sidebarItemLabel}
					data-flx="app.settings-modal-layout.settings-modal-sidebar-item.sidebar-item-label"
				>
					{label}
				</span>
				{expandable && (
					<CaretRightIcon
						className={styles.sidebarItemChevron}
						size={remFromPx(16)}
						weight="bold"
						aria-hidden="true"
						data-flx="app.settings-modal-layout.settings-modal-sidebar-item.sidebar-item-chevron"
					/>
				)}
			</>
		);
		return (
			<FocusRing
				offset={-2}
				focusClassName={styles.sidebarItemFocused}
				data-flx="app.settings-modal-layout.settings-modal-sidebar-item.focus-ring"
			>
				{isTabRole ? (
					<button
						data-flx="app.settings-modal-layout.settings-modal-sidebar-item.button"
						{...sharedProps}
						role="treeitem"
						aria-level={1}
						data-settings-tab="true"
						data-selected={selected ? 'true' : undefined}
						data-auto-select-on-keyboard-navigation={autoSelectOnKeyboardNavigation ? undefined : 'false'}
						data-expandable={expandable ? 'true' : undefined}
						data-tab-id={expandable ? expandableId : undefined}
						aria-selected={Boolean(selected)}
						aria-expanded={expandable ? expanded : undefined}
						aria-controls={controlsId}
						aria-owns={expandable && expanded ? sectionsGroupId : undefined}
						aria-keyshortcuts={expandable ? 'ArrowRight ArrowLeft' : undefined}
					>
						{content}
					</button>
				) : (
					<button
						data-flx="app.settings-modal-layout.settings-modal-sidebar-item.button--2"
						{...sharedProps}
						role="treeitem"
						aria-level={1}
						aria-controls={controlsId}
					>
						{content}
					</button>
				)}
			</FocusRing>
		);
	},
);
export const SettingsModalSidebarFooter: React.FC<{children: React.ReactNode}> = observer(({children}) => {
	return (
		<div
			className={styles.sidebarFooter}
			data-flx="app.settings-modal-layout.settings-modal-sidebar-footer.sidebar-footer"
		>
			{children}
		</div>
	);
});

export interface SettingsModalSidebarSubItemProps {
	label: React.ReactNode;
	sectionId: string;
	isActive: boolean;
	onClick: () => void;
}

export const SettingsModalSidebarSubItem: React.FC<SettingsModalSidebarSubItemProps> = observer(
	({label, sectionId, isActive, onClick}) => {
		return (
			<FocusRing offset={-2} data-flx="app.settings-modal-layout.settings-modal-sidebar-sub-item.focus-ring">
				<button
					type="button"
					className={clsx(styles.sidebarSubItem, isActive && styles.sidebarSubItemActive)}
					onClick={onClick}
					role="treeitem"
					aria-level={2}
					aria-selected={isActive}
					tabIndex={-1}
					data-settings-sidebar-item="true"
					data-section-id={sectionId}
					data-flx="app.settings-modal-layout.settings-modal-sidebar-sub-item.sidebar-sub-item.click.button"
				>
					<span
						className={styles.sidebarSubItemIndicator}
						data-flx="app.settings-modal-layout.settings-modal-sidebar-sub-item.sidebar-sub-item-indicator"
					/>
					<span
						className={styles.sidebarSubItemLabel}
						data-flx="app.settings-modal-layout.settings-modal-sidebar-sub-item.sidebar-sub-item-label"
					>
						{label}
					</span>
				</button>
			</FocusRing>
		);
	},
);

export interface SettingsModalSidebarSubItemsProps {
	children: React.ReactNode;
	expanded: boolean;
	groupId: string;
	labelledBy?: string;
}

export const SettingsModalSidebarSubItems: React.FC<SettingsModalSidebarSubItemsProps> = observer(
	({children, expanded, groupId, labelledBy}) => {
		const containerRef = useRef<HTMLDivElement>(null);
		const childrenArray = React.Children.toArray(children);
		const reducedMotion = Accessibility.useReducedMotion;
		const activeIndex = childrenArray.findIndex((child) => {
			if (!React.isValidElement(child)) return false;
			const element = child as React.ReactElement<{isActive?: boolean}>;
			return element.props?.isActive === true;
		});
		const hasActive = activeIndex !== -1;
		useEffect(() => {
			if (!expanded || !containerRef.current || !hasActive) return;
			const container = containerRef.current;
			const childElements = Array.from(container.children) as Array<HTMLElement>;
			const activeElement = childElements[activeIndex] as HTMLElement;
			if (!activeElement) return;
			const containerRect = container.getBoundingClientRect();
			const activeRect = activeElement.getBoundingClientRect();
			const top = activeRect.top - containerRect.top;
			const height = activeRect.height;
			container.style.setProperty('--active-top', `${top}px`);
			container.style.setProperty('--active-height', `${height}px`);
		}, [activeIndex, expanded, hasActive, children]);
		return (
			<div
				className={styles.sidebarSubItemsWrap}
				data-expanded={expanded ? 'true' : 'false'}
				data-reduced-motion={reducedMotion}
				data-flx="app.settings-modal-layout.settings-modal-sidebar-sub-items.sidebar-sub-items-wrap"
			>
				<div
					ref={containerRef}
					id={groupId}
					role="group"
					aria-labelledby={labelledBy}
					aria-hidden={!expanded}
					inert={!expanded}
					className={styles.sidebarSubItems}
					data-has-active={hasActive && expanded}
					data-reduced-motion={reducedMotion}
					style={
						hasActive
							? ({
									'--active-top': '0px',
									'--active-height': '0px',
								} as React.CSSProperties)
							: undefined
					}
					data-flx="app.settings-modal-layout.settings-modal-sidebar-sub-items.sidebar-sub-items"
				>
					{children}
				</div>
			</div>
		);
	},
);
export const settingsModalStyles = styles;
