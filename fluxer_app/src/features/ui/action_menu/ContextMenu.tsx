// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {getBoundaryPadding} from '@app/features/app/hooks/useAntiShiftFloating';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import styles from '@app/features/ui/action_menu/ContextMenu.module.css';
import {Scroller} from '@app/features/ui/components/Scroller';
import {PortalHostContext, resolvePortalHost, usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import type {ContextMenu as ContextMenuType} from '@app/features/ui/state/ContextMenu';
import ContextMenuState, {isContextMenuNodeTarget} from '@app/features/ui/state/ContextMenu';
import LayerManager from '@app/features/ui/state/LayerManager';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {isScrollbarDragActive} from '@app/features/ui/utils/ScrollbarDragState';
import {canUseWindowFocusedHoverControls} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import {wasPointerDownInside} from '@app/lib/overlay/DismissGuard';
import {ContextMenu as BaseContextMenu} from '@base-ui/react/context-menu';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useContext, useEffect, useId, useMemo, useRef, useState} from 'react';

const CONTEXT_MENU_DESCRIPTOR = msg({
	message: 'Context menu',
	comment: 'Accessible label for a context menu container.',
});
const SUBMENU_DESCRIPTOR = msg({
	message: 'Submenu',
	comment: 'Accessible label for a context menu submenu container.',
});

const ContextMenuCloseContext = React.createContext<() => void>(() => {});
const SubMenuDepthContext = React.createContext(0);

export const useContextMenuClose = () => useContext(ContextMenuCloseContext);
export const ContextMenuCloseProvider = ContextMenuCloseContext.Provider;

export type ContextMenuActionEvent =
	| React.MouseEvent<HTMLElement>
	| {
			readonly shiftKey?: boolean;
	  };
export type ContextMenuActionHandler = {bivarianceHack(event: ContextMenuActionEvent): void}['bivarianceHack'];

function stopHoverEventWhenWindowCannotUseHover(event: React.SyntheticEvent): void {
	const eventRoot = (event.currentTarget as HTMLElement | null)?.ownerDocument?.documentElement;
	if (canUseWindowFocusedHoverControls(eventRoot ?? document.documentElement)) return;
	event.stopPropagation();
}

const menuItemSelector = [
	'[role="menuitem"]:not([aria-disabled="true"]):not([data-disabled])',
	'[role="menuitemcheckbox"]:not([aria-disabled="true"]):not([data-disabled])',
	'[role="menuitemradio"]:not([aria-disabled="true"]):not([data-disabled])',
].join(', ');

type ContextMenuAlign = NonNullable<NonNullable<ContextMenuType['config']>['align']>;
type ContextMenuPlacement = {
	side: 'top' | 'bottom';
	align: 'start' | 'end';
};
interface ContextMenuVirtualElement {
	getBoundingClientRect: () => DOMRect;
}

const CONTEXT_MENU_EDGE_PADDING = 12;
const CONTEXT_MENU_OFFSET = 4;
const SUBMENU_OFFSET = 10;

const textValueFromNode = (node: React.ReactNode): string => {
	if (typeof node === 'string') return node;
	if (typeof node === 'number') return String(node);
	if (Array.isArray(node)) return node.map(textValueFromNode).join('');
	if (React.isValidElement(node)) {
		return textValueFromNode((node.props as {children?: React.ReactNode}).children);
	}
	return '';
};

function getPlacementForAlign(align: ContextMenuAlign): ContextMenuPlacement {
	switch (align) {
		case 'bottom-left':
			return {side: 'top', align: 'start'};
		case 'bottom-right':
			return {side: 'top', align: 'end'};
		case 'top-right':
			return {side: 'bottom', align: 'end'};
		default:
			return {side: 'bottom', align: 'start'};
	}
}

function createPointAnchor(contextMenu: ContextMenuType): ContextMenuVirtualElement {
	let anchorOffset: {dx: number; dy: number} | null = null;
	return {
		getBoundingClientRect() {
			const {x, y, target} = contextMenu.target;
			if (contextMenu.config?.trackDynamicPosition && isContextMenuNodeTarget(target) && target.isConnected) {
				const rect = target.getBoundingClientRect();
				if (!anchorOffset) {
					anchorOffset = {dx: x - rect.left, dy: y - rect.top};
				}
				return DOMRect.fromRect({
					width: 0,
					height: 0,
					x: rect.left + anchorOffset.dx,
					y: rect.top + anchorOffset.dy,
				});
			}
			return DOMRect.fromRect({width: 0, height: 0, x, y});
		},
	};
}

interface ReactiveMenuContentProps {
	render: () => React.ReactNode;
}

const ReactiveMenuContent: React.FC<ReactiveMenuContentProps> = observer(({render}) => {
	return <>{render()}</>;
});

ReactiveMenuContent.displayName = 'ReactiveMenuContent';

interface RootContextMenuProps {
	contextMenu: ContextMenuType;
}

function resolveContextMenuPortalHost(contextMenu: ContextMenuType, portalHost: HTMLElement | null): HTMLElement {
	const targetNode = contextMenu.target.target;
	if (isContextMenuNodeTarget(targetNode)) {
		const targetDocument = targetNode.ownerDocument;
		if (targetDocument && targetDocument !== document) {
			return targetDocument.body;
		}
	}
	return resolvePortalHost(portalHost);
}

const RootContextMenuInner: React.FC<RootContextMenuProps> = observer(({contextMenu}) => {
	const {i18n} = useLingui();
	const portalHost = usePortalHost();
	const [isOpen, setIsOpen] = useState(true);
	const menuElementRef = useRef<HTMLDivElement>(null);
	const anchor = useMemo(() => createPointAnchor(contextMenu), [contextMenu]);
	const placement = getPlacementForAlign(contextMenu.config?.align ?? 'top-left');

	const close = useCallback(() => {
		setIsOpen(false);
		ContextMenuState.closeById(contextMenu.id);
	}, [contextMenu.id]);

	const handleOpenChange = useCallback(
		(open: boolean) => {
			if (open) {
				setIsOpen(true);
				return;
			}
			close();
		},
		[close],
	);

	useEffect(() => {
		if (!isOpen) {
			return;
		}
		LayerManager.addLayer('contextmenu', contextMenu.id, close);
		return () => {
			LayerManager.removeLayer('contextmenu', contextMenu.id);
		};
	}, [close, contextMenu.id, isOpen]);

	const handleBackdropMouseDown = useCallback(
		(event: React.MouseEvent<HTMLDivElement>) => {
			if (isScrollbarDragActive() || wasPointerDownInside(menuElementRef.current)) {
				return;
			}
			if (contextMenu.config?.onBackdropMouseDown?.(event)) {
				event.preventDefault();
				event.stopPropagation();
			}
		},
		[contextMenu.config],
	);

	const handleBackdropClick = useCallback(() => {
		if (isScrollbarDragActive() || wasPointerDownInside(menuElementRef.current)) {
			return;
		}
		close();
	}, [close]);

	const handlePopupKeyDown = useCallback((event: React.KeyboardEvent<HTMLDivElement>) => {
		const eventTarget = event.target;
		const ownerWindow = event.currentTarget.ownerDocument.defaultView;
		const isEditableTarget =
			ownerWindow != null &&
			eventTarget instanceof ownerWindow.HTMLElement &&
			(eventTarget instanceof ownerWindow.HTMLInputElement ||
				eventTarget instanceof ownerWindow.HTMLTextAreaElement ||
				eventTarget.isContentEditable);
		if (isEditableTarget || event.key.length !== 1 || event.ctrlKey || event.metaKey || event.altKey) {
			return;
		}
		const menuElement = menuElementRef.current;
		if (!menuElement) {
			return;
		}
		const pressedKey = event.key.toLowerCase();
		const menuItems = menuElement.querySelectorAll<HTMLElement>(menuItemSelector);
		for (const item of menuItems) {
			const itemShortcut = item.dataset.menuShortcut?.toLowerCase().trim();
			const shortcutElement =
				item.querySelector(`.${styles.itemShortcut}`) || item.querySelector(`.${styles.itemShortcutHidden}`);
			const shortcutText = itemShortcut || shortcutElement?.textContent?.toLowerCase().trim();
			if (shortcutText === pressedKey) {
				event.preventDefault();
				event.stopPropagation();
				item.focus({preventScroll: true});
				item.click();
				return;
			}
		}
	}, []);

	if (!isOpen) {
		return null;
	}

	const resolvedPortalHost = resolveContextMenuPortalHost(contextMenu, portalHost);
	return (
		<BaseContextMenu.Root open={isOpen} onOpenChange={handleOpenChange} data-flx="ui.action-menu.context-menu.root">
			<BaseContextMenu.Portal container={resolvedPortalHost} data-flx="ui.action-menu.context-menu.portal">
				<BaseContextMenu.Backdrop
					className={styles.backdrop}
					onMouseDown={handleBackdropMouseDown}
					onClick={handleBackdropClick}
					data-flx="ui.action-menu.context-menu.backdrop"
				/>
				<BaseContextMenu.Positioner
					anchor={anchor}
					side={placement.side}
					align={placement.align}
					sideOffset={CONTEXT_MENU_OFFSET}
					alignOffset={0}
					collisionPadding={getBoundaryPadding(
						resolvedPortalHost?.ownerDocument ?? document,
						CONTEXT_MENU_EDGE_PADDING,
					)}
					className={styles.contextMenuPositioner}
					data-flx="ui.action-menu.context-menu.positioner"
				>
					<BaseContextMenu.Popup
						ref={menuElementRef}
						finalFocus={false}
						aria-label={i18n._(CONTEXT_MENU_DESCRIPTOR)}
						data-context-menu-root="true"
						className={styles.contextMenu}
						onKeyDown={handlePopupKeyDown}
						onMouseMoveCapture={stopHoverEventWhenWindowCannotUseHover}
						onMouseOverCapture={stopHoverEventWhenWindowCannotUseHover}
						onPointerMoveCapture={stopHoverEventWhenWindowCannotUseHover}
						onPointerOverCapture={stopHoverEventWhenWindowCannotUseHover}
						data-flx="ui.action-menu.context-menu.popup"
					>
						<PortalHostContext.Provider value={resolvedPortalHost}>
							<ContextMenuCloseContext.Provider value={close}>
								<Scroller
									className={styles.menuScroller}
									contentClassName={styles.menuScrollerContent}
									overflow="auto"
									fade={false}
									key={`context-menu-scroller-${contextMenu.id}`}
									data-flx="ui.action-menu.context-menu.menu-scroller"
								>
									<ReactiveMenuContent
										render={() => contextMenu.render({onClose: close})}
										data-flx="ui.action-menu.context-menu.reactive-menu-content"
									/>
								</Scroller>
							</ContextMenuCloseContext.Provider>
						</PortalHostContext.Provider>
					</BaseContextMenu.Popup>
				</BaseContextMenu.Positioner>
			</BaseContextMenu.Portal>
		</BaseContextMenu.Root>
	);
});

export const RootContextMenu: React.FC<RootContextMenuProps> = observer(({contextMenu}) => {
	return <RootContextMenuInner contextMenu={contextMenu} data-flx="ui.action-menu.context-menu.root-inner" />;
});

interface SelectableMenuItemProps {
	selectionMode: 'single' | 'multiple';
	selected: boolean;
	onAction: () => void;
	isDisabled?: boolean;
	shouldCloseOnSelect?: boolean;
	className?: string;
	textValue: string;
	children: React.ReactNode;
}

export const SelectableMenuItem = React.forwardRef<HTMLDivElement, SelectableMenuItemProps>(
	(
		{selectionMode, selected, onAction, isDisabled, shouldCloseOnSelect, className, textValue, children},
		forwardedRef,
	) => {
		const itemKey = useId();
		const closeMenu = useContext(ContextMenuCloseContext);
		const handleAction = useCallback(() => {
			if (isDisabled) return;
			onAction();
			if (shouldCloseOnSelect) {
				closeMenu();
			}
		}, [closeMenu, isDisabled, onAction, shouldCloseOnSelect]);

		if (selectionMode === 'multiple') {
			return (
				<BaseContextMenu.CheckboxItem
					ref={forwardedRef}
					checked={selected}
					onCheckedChange={handleAction}
					disabled={isDisabled}
					closeOnClick={false}
					className={className}
					label={textValue}
					data-flx="ui.action-menu.context-menu.selectable-menu-item.checkbox-item"
				>
					{children}
				</BaseContextMenu.CheckboxItem>
			);
		}

		return (
			<BaseContextMenu.RadioGroup
				value={selected ? itemKey : null}
				className={styles.selectableSection}
				data-flx="ui.action-menu.context-menu.selectable-menu-item.radio-group"
			>
				<BaseContextMenu.RadioItem
					ref={forwardedRef}
					value={itemKey}
					onClick={handleAction}
					disabled={isDisabled}
					closeOnClick={false}
					className={className}
					label={textValue}
					data-flx="ui.action-menu.context-menu.selectable-menu-item.radio-item"
				>
					{children}
				</BaseContextMenu.RadioItem>
			</BaseContextMenu.RadioGroup>
		);
	},
);

SelectableMenuItem.displayName = 'SelectableMenuItem';

interface MenuItemProps {
	label: string;
	disabled?: boolean;
	onClick?: ContextMenuActionHandler;
	onSelect?: ContextMenuActionHandler;
	icon?: React.ReactNode;
	danger?: boolean;
	color?: string;
	className?: string;
	children?: React.ReactNode;
	closeOnSelect?: boolean;
	shortcut?: React.ReactNode;
	hint?: React.ReactNode;
}

export const MenuItem = React.forwardRef<HTMLDivElement, MenuItemProps>(
	(
		{label, disabled, onSelect, icon, danger, className, children, closeOnSelect = true, shortcut, hint},
		forwardedRef,
	) => {
		const closeMenu = useContext(ContextMenuCloseContext);
		const shouldShowShortcuts = Accessibility.showContextMenuShortcuts;
		const hasShortcut = Boolean(shortcut);
		const shouldShowShortcut = hasShortcut && shouldShowShortcuts;
		const shortcutKey = typeof shortcut === 'string' && shortcut.length === 1 ? shortcut : undefined;
		const inFlightRef = useRef(false);
		const handleClick = useCallback(
			(event: React.MouseEvent<HTMLElement>) => {
				if (disabled || inFlightRef.current) return;
				inFlightRef.current = true;
				try {
					onSelect?.(event);
				} finally {
					if (closeOnSelect) {
						closeMenu();
					}
					queueMicrotask(() => {
						inFlightRef.current = false;
					});
				}
			},
			[closeMenu, closeOnSelect, disabled, onSelect],
		);
		const labelContent = children ?? label;
		return (
			<BaseContextMenu.Item
				ref={forwardedRef}
				onClick={handleClick}
				disabled={disabled}
				closeOnClick={false}
				className={clsx(styles.item, className, {
					[styles.danger]: danger,
					[styles.disabled]: disabled,
					[styles.itemWithShortcut]: shouldShowShortcut,
				})}
				label={label || textValueFromNode(children)}
				data-menu-shortcut={shortcutKey}
				data-flx="ui.action-menu.context-menu.menu-item.item"
			>
				<div
					className={hint ? styles.itemLabelContainer : styles.itemLabel}
					data-flx="ui.action-menu.context-menu.menu-item.item-label"
				>
					{hint ? (
						<>
							<div className={styles.itemLabelText} data-flx="ui.action-menu.context-menu.menu-item.item-label-text">
								{labelContent}
							</div>
							<div className={styles.itemHint} data-flx="ui.action-menu.context-menu.menu-item.item-hint">
								{hint}
							</div>
						</>
					) : (
						<>
							{labelContent}
							{hasShortcut && (
								<span
									className={shouldShowShortcut ? styles.itemShortcut : styles.itemShortcutHidden}
									aria-hidden={shouldShowShortcut ? undefined : true}
									data-flx="ui.action-menu.context-menu.menu-item.item-shortcut"
								>
									{shortcut}
								</span>
							)}
						</>
					)}
				</div>
				{icon && !shouldShowShortcut && (
					<div className={styles.itemIcon} data-flx="ui.action-menu.context-menu.menu-item.item-icon">
						{icon}
					</div>
				)}
			</BaseContextMenu.Item>
		);
	},
);

MenuItem.displayName = 'MenuItem';

interface SubMenuProps {
	label: string;
	disabled?: boolean;
	hint?: string;
	danger?: boolean;
	children?: React.ReactNode;
	render?: () => React.ReactNode;
	onTriggerSelect?: () => void;
}

export const SubMenu = React.forwardRef<HTMLDivElement, SubMenuProps>(
	({label, disabled, hint, danger = false, children, render, onTriggerSelect}, forwardedRef) => {
		const {i18n} = useLingui();
		const portalHost = usePortalHost();
		const submenuDepth = useContext(SubMenuDepthContext);
		const [isOpen, setIsOpen] = useState(false);
		const portalContainer = submenuDepth === 0 ? resolvePortalHost(portalHost) : undefined;

		const handleLabelClick = useCallback(
			(event: React.MouseEvent<HTMLDivElement>) => {
				if (disabled || !onTriggerSelect) return;
				const target = event.target as HTMLElement;
				if (target.closest('[data-submenu-caret="true"]')) {
					return;
				}
				event.preventDefault();
				event.stopPropagation();
				onTriggerSelect();
			},
			[disabled, onTriggerSelect],
		);

		const handleLabelKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLDivElement>) => {
				if (disabled || !onTriggerSelect) return;
				if (isKeyboardActivationKey(event.key)) {
					event.preventDefault();
					event.stopPropagation();
					onTriggerSelect();
				}
			},
			[disabled, onTriggerSelect],
		);

		return (
			<BaseContextMenu.SubmenuRoot
				open={isOpen}
				onOpenChange={setIsOpen}
				data-flx="ui.action-menu.context-menu.sub-menu.base-context-menu-submenu-root"
			>
				<BaseContextMenu.SubmenuTrigger
					ref={forwardedRef}
					disabled={disabled}
					openOnHover
					className={clsx(styles.item, {
						[styles.disabled]: disabled,
						[styles.danger]: danger,
					})}
					label={label}
					data-flx="ui.action-menu.context-menu.sub-menu.trigger"
				>
					<div
						className={styles.itemLabelContainer}
						onClick={handleLabelClick}
						onKeyDown={handleLabelKeyDown}
						role="button"
						tabIndex={-1}
						data-flx="ui.action-menu.context-menu.sub-menu.item-label-container"
					>
						<div className={styles.itemLabelText} data-flx="ui.action-menu.context-menu.sub-menu.item-label-text">
							{label}
						</div>
						{hint && (
							<div className={styles.itemHint} data-flx="ui.action-menu.context-menu.sub-menu.item-hint">
								{hint}
							</div>
						)}
					</div>
					<svg
						className={styles.submenuCaret}
						width="16"
						height="16"
						viewBox="0 0 256 256"
						aria-hidden="true"
						data-submenu-caret="true"
						data-flx="ui.action-menu.context-menu.sub-menu.submenu-caret"
					>
						<path
							fill="currentColor"
							d="M184.49 136.49l-80 80a12 12 0 0 1-17-17L159 128L87.51 56.49a12 12 0 1 1 17-17l80 80a12 12 0 0 1-.02 17"
							data-flx="ui.action-menu.context-menu.sub-menu.path"
						/>
					</svg>
				</BaseContextMenu.SubmenuTrigger>
				<BaseContextMenu.Portal container={portalContainer} data-flx="ui.action-menu.context-menu.sub-menu.portal">
					<BaseContextMenu.Positioner
						sideOffset={SUBMENU_OFFSET}
						className={styles.contextMenuPositioner}
						collisionPadding={getBoundaryPadding(portalHost?.ownerDocument ?? document, CONTEXT_MENU_EDGE_PADDING)}
						data-flx="ui.action-menu.context-menu.sub-menu.positioner"
					>
						<BaseContextMenu.Popup
							finalFocus={false}
							aria-label={i18n._(SUBMENU_DESCRIPTOR)}
							className={styles.submenuPopover}
							onMouseMoveCapture={stopHoverEventWhenWindowCannotUseHover}
							onMouseOverCapture={stopHoverEventWhenWindowCannotUseHover}
							onPointerMoveCapture={stopHoverEventWhenWindowCannotUseHover}
							onPointerOverCapture={stopHoverEventWhenWindowCannotUseHover}
							data-flx="ui.action-menu.context-menu.sub-menu.popup"
						>
							{isOpen ? (
								<SubMenuDepthContext.Provider value={submenuDepth + 1}>
									<Scroller
										className={styles.submenuScroller}
										contentClassName={styles.menuScrollerContent}
										overflow="auto"
										fade={false}
										key={`context-submenu-scroller-${label}`}
										data-flx="ui.action-menu.context-menu.sub-menu.submenu-scroller"
									>
										{render ? (
											<ReactiveMenuContent
												render={render}
												data-flx="ui.action-menu.context-menu.sub-menu.reactive-menu-content"
											/>
										) : (
											children
										)}
									</Scroller>
								</SubMenuDepthContext.Provider>
							) : null}
						</BaseContextMenu.Popup>
					</BaseContextMenu.Positioner>
				</BaseContextMenu.Portal>
			</BaseContextMenu.SubmenuRoot>
		);
	},
);

SubMenu.displayName = 'SubMenu';

export const MenuSeparator: React.FC = observer(() => {
	return <BaseContextMenu.Separator className={styles.separator} data-flx="ui.action-menu.context-menu.separator" />;
});

interface CheckboxItemProps {
	label?: string;
	checked: boolean;
	onCheckedChange: (checked: boolean) => void;
	disabled?: boolean;
	icon?: React.ReactNode;
	children?: React.ReactNode;
	danger?: boolean;
	closeOnChange?: boolean;
}

export const CheckboxItem = React.forwardRef<HTMLDivElement, CheckboxItemProps>(
	(
		{label, checked, onCheckedChange, disabled, icon: _icon, children, danger = false, closeOnChange = false},
		forwardedRef,
	) => {
		const closeMenu = useContext(ContextMenuCloseContext);
		const handleCheckedChange = useCallback(
			(nextChecked: boolean) => {
				if (disabled) return;
				onCheckedChange(nextChecked);
				if (closeOnChange) {
					closeMenu();
				}
			},
			[closeMenu, closeOnChange, disabled, onCheckedChange],
		);
		return (
			<BaseContextMenu.CheckboxItem
				ref={forwardedRef}
				checked={checked}
				onCheckedChange={handleCheckedChange}
				disabled={disabled}
				closeOnClick={false}
				className={clsx(styles.item, styles.checkboxItem, {
					[styles.disabled]: disabled,
					[styles.danger]: danger,
				})}
				label={label || textValueFromNode(children)}
				data-flx="ui.action-menu.context-menu.checkbox-item.item"
			>
				<div className={styles.itemLabel} data-flx="ui.action-menu.context-menu.checkbox-item.item-label">
					{children || label}
				</div>
				<BaseContextMenu.CheckboxItemIndicator
					className={styles.checkboxIndicator}
					keepMounted
					aria-hidden="true"
					data-flx="ui.action-menu.context-menu.checkbox-item.checkbox-indicator"
				>
					<div
						className={clsx(styles.checkbox, {
							[styles.checkboxChecked]: checked,
						})}
						data-flx="ui.action-menu.context-menu.checkbox-item.checkbox"
					/>
				</BaseContextMenu.CheckboxItemIndicator>
			</BaseContextMenu.CheckboxItem>
		);
	},
);

CheckboxItem.displayName = 'CheckboxItem';

interface MenuGroupProps {
	label?: string;
	children?: React.ReactNode;
}

export const MenuGroup: React.FC<MenuGroupProps> = observer(({children}) => {
	const validChildren = React.Children.toArray(children).filter((child): child is React.ReactElement => {
		if (!React.isValidElement(child)) return false;
		if (child.type === React.Fragment && !(child.props as {children?: React.ReactNode}).children) return false;
		return true;
	});
	if (validChildren.length === 0) {
		return null;
	}
	return (
		<BaseContextMenu.Group className={styles.group} data-flx="ui.action-menu.context-menu.menu-group.group">
			{validChildren}
		</BaseContextMenu.Group>
	);
});

interface MenuGroupLabelProps extends React.HTMLAttributes<HTMLElement> {
	children?: React.ReactNode;
}

export const MenuGroupLabel: React.FC<MenuGroupLabelProps> = observer(({children, className, ...props}) => {
	const dataFlx = (props as {'data-flx'?: string})['data-flx'] ?? 'ui.action-menu.context-menu.menu-group-label.header';
	return (
		<BaseContextMenu.GroupLabel {...props} className={clsx(styles.groupLabel, className)} data-flx={dataFlx}>
			{children}
		</BaseContextMenu.GroupLabel>
	);
});

interface ContextMenuProps {
	ownerDocument?: Document;
}

export const ContextMenu: React.FC<ContextMenuProps> = observer(({ownerDocument}) => {
	const portalHost = usePortalHost();
	if (isMobileExperienceEnabled()) {
		return null;
	}
	const scopeDocument = ownerDocument ?? portalHost?.ownerDocument ?? document;
	const contextMenu = ContextMenuState.getContextMenu(scopeDocument);
	if (!contextMenu) return null;
	return <RootContextMenu key={contextMenu.id} contextMenu={contextMenu} data-flx="ui.action-menu.context-menu.root" />;
});
