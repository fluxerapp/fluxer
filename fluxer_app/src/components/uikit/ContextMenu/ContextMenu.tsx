/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import type {PressEvent} from 'react-aria-components';
import {
	Menu as AriaMenu,
	MenuItem as AriaMenuItem,
	Popover as AriaPopover,
	MenuSection as AriaSection,
	Separator as AriaSeparator,
	SubmenuTrigger as AriaSubmenuTrigger,
} from 'react-aria-components';
import {createPortal} from 'react-dom';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import type {ContextMenu as ContextMenuType} from '~/stores/ContextMenuStore';
import ContextMenuStore from '~/stores/ContextMenuStore';
import LayerManager from '~/stores/LayerManager';
import {isMobileExperienceEnabled} from '~/utils/mobileExperience';
import {isScrollbarDragActive} from '~/utils/ScrollbarDragState';
import styles from './ContextMenu.module.css';

const ContextMenuCloseContext = React.createContext<() => void>(() => {});

export const useContextMenuClose = () => React.useContext(ContextMenuCloseContext);

export const ContextMenuCloseProvider = ContextMenuCloseContext.Provider;

interface RootContextMenuProps {
	contextMenu: ContextMenuType;
}

const RootContextMenuInner: React.FC<RootContextMenuProps> = observer(({contextMenu}) => {
	const [isOpen, setIsOpen] = React.useState(true);
	const [isPositioned, setIsPositioned] = React.useState(false);
	const [position, setPosition] = React.useState({x: 0, y: 0});
	const menuRef = React.useRef<HTMLDivElement>(null);
	const menuContentRef = React.useRef<HTMLDivElement>(null);
	const rafIdRef = React.useRef<number | null>(null);
	const focusFirstMenuItem = React.useCallback(() => {
		const menuElement = menuContentRef.current;
		if (!menuElement) {
			return;
		}

		const firstInteractable = menuElement.querySelector<HTMLElement>(
			[
				'[role="menuitem"]:not([aria-disabled="true"])',
				'[role="menuitemcheckbox"]:not([aria-disabled="true"])',
				'[role="menuitemradio"]:not([aria-disabled="true"])',
			].join(', '),
		);

		(firstInteractable ?? menuElement).focus({preventScroll: true});
	}, []);

	const close = React.useCallback(() => {
		setIsOpen(false);
		ContextMenuActionCreators.close();
	}, []);

	React.useLayoutEffect(() => {
		const {x, y} = contextMenu.target;
		const align = contextMenu.config?.align ?? 'top-left';

		if (rafIdRef.current !== null) {
			cancelAnimationFrame(rafIdRef.current);
		}

		rafIdRef.current = requestAnimationFrame(() => {
			if (menuRef.current) {
				const rect = menuRef.current.getBoundingClientRect();
				const viewportWidth = window.innerWidth;
				const viewportHeight = window.innerHeight;

				const cursorOffset = 4;
				const edgePadding = 12;

				let finalX: number;
				let finalY = y + cursorOffset;

				if (align === 'top-right') {
					finalX = x - rect.width;
					if (finalX < edgePadding) {
						finalX = x + cursorOffset;
						if (finalX + rect.width > viewportWidth - edgePadding) {
							finalX = Math.max(edgePadding, viewportWidth - rect.width - edgePadding);
						}
					}
				} else {
					finalX = x + cursorOffset;
					if (finalX + rect.width > viewportWidth - edgePadding) {
						finalX = x - rect.width - cursorOffset;
						if (finalX < edgePadding) {
							finalX = Math.max(edgePadding, viewportWidth - rect.width - edgePadding);
						}
					}
				}

				if (finalY + rect.height > viewportHeight - edgePadding) {
					finalY = y - rect.height - cursorOffset;
					if (finalY < edgePadding) {
						finalY = Math.max(edgePadding, viewportHeight - rect.height - edgePadding);
					}
				}

				finalX = Math.max(edgePadding, finalX);
				finalY = Math.max(edgePadding, finalY);

				setPosition({x: finalX, y: finalY});
			}
			setIsPositioned(true);
			rafIdRef.current = null;
		});

		return () => {
			if (rafIdRef.current !== null) {
				cancelAnimationFrame(rafIdRef.current);
				rafIdRef.current = null;
			}
		};
	}, [contextMenu.target, contextMenu.config?.align]);

	React.useEffect(() => {
		if (isOpen) {
			LayerManager.addLayer('contextmenu', contextMenu.id, close);
			return () => {
				LayerManager.removeLayer('contextmenu', contextMenu.id);
			};
		}
		return;
	}, [isOpen, contextMenu.id, close]);

	const handleBackdropClick = React.useCallback(() => {
		if (isScrollbarDragActive()) {
			return;
		}

		close();
	}, [close]);

	React.useLayoutEffect(() => {
		if (!isOpen || !isPositioned) {
			return;
		}

		focusFirstMenuItem();
	}, [isOpen, isPositioned, contextMenu.id, focusFirstMenuItem]);

	React.useEffect(() => {
		const handleKeyDown = (e: KeyboardEvent) => {
			if (e.key === 'Escape') {
				close();
				return;
			}

			if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
				const menuElement = menuContentRef.current;
				if (!menuElement) return;

				const menuItems = menuElement.querySelectorAll<HTMLElement>('[role="menuitem"]');
				const pressedKey = e.key.toLowerCase();

				for (const item of menuItems) {
					const shortcutElement =
						item.querySelector(`.${styles.itemShortcut}`) || item.querySelector('[class*="shortcut"]');
					if (shortcutElement) {
						const shortcutText = shortcutElement.textContent?.toLowerCase().trim();
						if (shortcutText === pressedKey) {
							e.preventDefault();
							e.stopPropagation();
							item.click();
							return;
						}
					}
				}
			}
		};

		const handleClickOutside = (e: MouseEvent) => {
			if (isScrollbarDragActive()) {
				return;
			}

			if (menuRef.current && !menuRef.current.contains(e.target as Node)) {
				close();
			}
		};

		document.addEventListener('keydown', handleKeyDown);
		document.addEventListener('mousedown', handleClickOutside);

		return () => {
			document.removeEventListener('keydown', handleKeyDown);
			document.removeEventListener('mousedown', handleClickOutside);
		};
	}, [close]);

	if (!isOpen) {
		return null;
	}

	const {x, y} = contextMenu.target;

	return (
		<div className={styles.contextMenuOverlay} data-overlay-pass-through="true">
			<div className={styles.backdrop} onClick={handleBackdropClick} aria-hidden="true" />
			<div
				ref={menuRef}
				className={styles.contextMenu}
				style={{
					position: 'fixed',
					left: `${isPositioned ? position.x : x}px`,
					top: `${isPositioned ? position.y : y}px`,
					opacity: isPositioned ? 1 : 0,
					visibility: isPositioned ? 'visible' : 'hidden',
					pointerEvents: isPositioned ? 'auto' : 'none',
					zIndex: 'var(--z-index-contextmenu)',
				}}
			>
				<ContextMenuCloseContext.Provider value={close}>
					<AriaMenu ref={menuContentRef} className={styles.ariaMenu} aria-label="Context menu">
						{contextMenu.render({onClose: close})}
					</AriaMenu>
				</ContextMenuCloseContext.Provider>
			</div>
		</div>
	);
});

export const RootContextMenu: React.FC<RootContextMenuProps> = observer(({contextMenu}) => {
	return <RootContextMenuInner contextMenu={contextMenu} />;
});

interface MenuItemProps {
	label: string;
	disabled?: boolean;
	onClick?: (event: React.MouseEvent<HTMLButtonElement>) => void;
	onSelect?: (event: PressEvent) => void;
	icon?: React.ReactNode;
	danger?: boolean;
	color?: string;
	className?: string;
	children?: React.ReactNode;
	closeOnSelect?: boolean;
	shortcut?: string;
}

export const MenuItem = React.forwardRef<HTMLDivElement, MenuItemProps>(
	({label, disabled, onSelect, icon, danger, className, children, closeOnSelect = true, shortcut}, forwardedRef) => {
		const closeMenu = React.useContext(ContextMenuCloseContext);

		const handlePress = React.useCallback(
			(event: PressEvent) => {
				if (disabled) return;
				onSelect?.(event);
				if (closeOnSelect) {
					closeMenu();
				}
			},
			[closeMenu, closeOnSelect, disabled, onSelect],
		);

		return (
			<AriaMenuItem
				ref={forwardedRef}
				onPress={handlePress}
				isDisabled={disabled}
				className={clsx(styles.item, className, {
					[styles.danger]: danger,
					[styles.disabled]: disabled,
				})}
				textValue={label || (typeof children === 'string' ? children : '')}
			>
				{icon && <div className={styles.itemIcon}>{icon}</div>}
				<div className={styles.itemLabel}>{children || label}</div>
				{shortcut && <div className={styles.itemShortcut}>{shortcut}</div>}
			</AriaMenuItem>
		);
	},
);

MenuItem.displayName = 'MenuItem';

interface SubMenuProps {
	label: string;
	icon?: React.ReactNode;
	disabled?: boolean;
	hint?: string;
	children?: React.ReactNode;
	onTriggerSelect?: () => void;
	selectionMode?: 'none' | 'single' | 'multiple';
}

export const SubMenu = React.forwardRef<HTMLDivElement, SubMenuProps>(
	({label, icon, disabled, hint, children, onTriggerSelect, selectionMode}, forwardedRef) => {
		const handleTriggerPress = React.useCallback(() => {
			if (disabled) return;
			onTriggerSelect?.();
		}, [disabled, onTriggerSelect]);

		const handleLabelClick = React.useCallback(
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

		const handleLabelKeyDown = React.useCallback(
			(event: React.KeyboardEvent<HTMLDivElement>) => {
				if (disabled || !onTriggerSelect) return;
				if (event.key === 'Enter' || event.key === ' ') {
					event.preventDefault();
					onTriggerSelect();
				}
			},
			[disabled, onTriggerSelect],
		);

		return (
			<AriaSubmenuTrigger>
				<AriaMenuItem
					ref={forwardedRef}
					isDisabled={disabled}
					className={clsx(styles.item, {
						[styles.disabled]: disabled,
					})}
					textValue={label}
					onAction={onTriggerSelect ? handleTriggerPress : undefined}
				>
					{icon && <div className={styles.itemIcon}>{icon}</div>}
					<div
						className={clsx(styles.itemLabelContainer)}
						onClick={handleLabelClick}
						onKeyDown={handleLabelKeyDown}
						role="button"
						tabIndex={-1}
					>
						<div className={styles.itemLabelText}>{label}</div>
						{hint && <div className={styles.itemHint}>{hint}</div>}
					</div>
					<svg
						className={styles.submenuCaret}
						width="16"
						height="16"
						viewBox="0 0 256 256"
						aria-hidden="true"
						data-submenu-caret="true"
					>
						<path
							fill="currentColor"
							d="M184.49 136.49l-80 80a12 12 0 0 1-17-17L159 128L87.51 56.49a12 12 0 1 1 17-17l80 80a12 12 0 0 1-.02 17"
						/>
					</svg>
				</AriaMenuItem>
				<AriaPopover placement="right top" offset={4} className={styles.submenuPopover}>
					<AriaMenu className={styles.ariaMenu} aria-label="Submenu" autoFocus="first" selectionMode={selectionMode}>
						{children}
					</AriaMenu>
				</AriaPopover>
			</AriaSubmenuTrigger>
		);
	},
);

SubMenu.displayName = 'SubMenu';

export const MenuSeparator: React.FC = observer(() => {
	return <AriaSeparator className={styles.separator} />;
});

interface CheckboxItemProps {
	label: string;
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
		{label, checked, onCheckedChange, disabled, icon, children, danger = false, closeOnChange = false},
		forwardedRef,
	) => {
		const closeMenu = React.useContext(ContextMenuCloseContext);

		const handleAction = React.useCallback(
			(_e: PressEvent) => {
				if (disabled) return;
				onCheckedChange(!checked);
				if (closeOnChange) {
					closeMenu();
				}
			},
			[checked, closeMenu, closeOnChange, disabled, onCheckedChange],
		);

		return (
			<AriaMenuItem
				ref={forwardedRef}
				onPress={handleAction}
				isDisabled={disabled}
				className={clsx(styles.item, styles.checkboxItem, {
					[styles.disabled]: disabled,
					[styles.danger]: danger,
				})}
				textValue={label || (typeof children === 'string' ? children : '')}
			>
				{icon && <div className={styles.itemIcon}>{icon}</div>}
				<div className={styles.itemLabel}>{children || label}</div>
				<div className={styles.checkboxIndicator}>
					<div
						className={clsx(styles.checkbox, {
							[styles.checkboxChecked]: checked,
						})}
					/>
				</div>
			</AriaMenuItem>
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

	return <AriaSection className={styles.group}>{validChildren}</AriaSection>;
});

export const ContextMenu: React.FC = observer(() => {
	if (isMobileExperienceEnabled()) {
		return null;
	}

	const contextMenu = ContextMenuStore.contextMenu;

	if (!contextMenu) return null;

	return createPortal(<RootContextMenu key={contextMenu.id} contextMenu={contextMenu} />, document.body);
});
