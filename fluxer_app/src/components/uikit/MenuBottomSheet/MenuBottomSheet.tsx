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
import type React from 'react';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Slider} from '~/components/uikit/Slider';
import {usePressable} from '~/hooks/usePressable';
import styles from './MenuBottomSheet.module.css';

export interface MenuItemType {
	id?: string;
	icon: React.ReactNode;
	label: string;
	onClick: () => void;
	danger?: boolean;
	disabled?: boolean;
}

interface MenuSliderType {
	label: string;
	value: number;
	minValue: number;
	maxValue: number;
	onChange: (value: number) => void;
	onFormat?: (value: number) => string;
	factoryDefaultValue?: number;
}

interface MenuCheckboxType {
	icon?: React.ReactNode;
	label: string;
	checked: boolean;
	onChange: (checked: boolean) => void;
	disabled?: boolean;
}

export interface MenuRadioType {
	label: string;
	subtext?: string;
	selected: boolean;
	onSelect: () => void;
	disabled?: boolean;
}

export interface MenuGroupType {
	items: Array<MenuItemType | MenuSliderType | MenuCheckboxType | MenuRadioType>;
}

interface MenuBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	title?: string;
	groups: Array<MenuGroupType>;
	headerContent?: React.ReactNode;
	showCloseButton?: boolean;
}

const MenuCheckboxItem: React.FC<{item: MenuCheckboxType; isLast: boolean}> = observer(({item, isLast}) => {
	const {isPressed, pressableProps} = usePressable(item.disabled);
	return (
		<>
			<button
				type="button"
				role="checkbox"
				aria-checked={item.checked}
				aria-label={item.label}
				onClick={() => item.onChange(!item.checked)}
				disabled={item.disabled}
				className={clsx(styles.menuItem, item.disabled && styles.disabled, isPressed && styles.pressed)}
				{...pressableProps}
			>
				{item.icon && (
					<div className={styles.iconContainer} aria-hidden="true">
						{item.icon}
					</div>
				)}
				<span className={styles.label}>{item.label}</span>
				<div className={styles.checkboxContainer} aria-hidden="true">
					<div className={clsx(styles.checkbox, item.checked && styles.checked)}>
						{item.checked && (
							<svg className={styles.checkIcon} viewBox="0 0 12 12" fill="none" aria-hidden="true">
								<path
									d="M10 3L4.5 8.5L2 6"
									stroke="currentColor"
									strokeWidth="2"
									strokeLinecap="round"
									strokeLinejoin="round"
								/>
							</svg>
						)}
					</div>
				</div>
			</button>
			{!isLast && <div className={styles.divider} />}
		</>
	);
});

const MenuRadioItem: React.FC<{item: MenuRadioType; isLast: boolean}> = observer(({item, isLast}) => {
	const {isPressed, pressableProps} = usePressable(item.disabled);
	return (
		<>
			<button
				type="button"
				role="radio"
				aria-checked={item.selected}
				aria-label={item.label}
				onClick={item.onSelect}
				disabled={item.disabled}
				className={clsx(styles.menuItem, item.disabled && styles.disabled, isPressed && styles.pressed)}
				{...pressableProps}
			>
				<div className={styles.radioContainer} aria-hidden="true">
					<div className={clsx(styles.radio, item.selected && styles.radioSelected)}>
						{item.selected && <div className={styles.radioInner} />}
					</div>
				</div>
				<div className={styles.labelColumn}>
					<span className={styles.label}>{item.label}</span>
					{item.subtext && <span className={styles.subtext}>{item.subtext}</span>}
				</div>
			</button>
			{!isLast && <div className={styles.divider} />}
		</>
	);
});

const MenuActionItem: React.FC<{item: MenuItemType; isLast: boolean}> = observer(({item, isLast}) => {
	const {isPressed, pressableProps} = usePressable(item.disabled);
	return (
		<>
			<button
				type="button"
				onClick={item.onClick}
				disabled={item.disabled}
				className={clsx(
					styles.menuItem,
					item.disabled && styles.disabled,
					item.danger && styles.danger,
					isPressed && styles.pressed,
					isPressed && item.danger && styles.pressedDanger,
				)}
				{...pressableProps}
			>
				<div className={styles.iconContainer}>{item.icon}</div>
				<span className={styles.label}>{item.label}</span>
			</button>
			{!isLast && <div className={styles.divider} />}
		</>
	);
});

const MenuSliderItem: React.FC<{item: MenuSliderType; isLast: boolean}> = observer(({item, isLast}) => {
	return (
		<>
			<div className={styles.sliderContainer}>
				<span className={styles.sliderLabel}>{item.label}</span>
				<Slider
					defaultValue={item.value}
					factoryDefaultValue={item.factoryDefaultValue ?? item.value}
					minValue={item.minValue}
					maxValue={item.maxValue}
					onValueChange={item.onChange}
					onValueRender={item.onFormat}
					value={item.value}
					mini={true}
				/>
			</div>
			{!isLast && <div className={styles.divider} />}
		</>
	);
});

const MenuItem: React.FC<{item: MenuItemType | MenuSliderType | MenuCheckboxType | MenuRadioType; isLast?: boolean}> =
	observer(({item, isLast = false}) => {
		if ('checked' in item) {
			return <MenuCheckboxItem item={item as MenuCheckboxType} isLast={isLast} />;
		}
		if ('selected' in item) {
			return <MenuRadioItem item={item as MenuRadioType} isLast={isLast} />;
		}
		if ('onClick' in item) {
			return <MenuActionItem item={item as MenuItemType} isLast={isLast} />;
		}
		return <MenuSliderItem item={item as MenuSliderType} isLast={isLast} />;
	});

const MenuGroup: React.FC<{group: MenuGroupType; isLast?: boolean}> = observer(({group, isLast = false}) => {
	return (
		<>
			<div className={styles.groupContainer}>
				{group.items.map((item, index) => (
					<MenuItem
						key={`${'label' in item ? item.label : 'slider'}-${index}`}
						item={item}
						isLast={index === group.items.length - 1}
					/>
				))}
			</div>
			{!isLast && <div className={styles.groupSpacer} />}
		</>
	);
});

export const MenuBottomSheet: React.FC<MenuBottomSheetProps> = observer(
	({isOpen, onClose, title, groups, headerContent, showCloseButton = false}) => {
		const hasHeader = Boolean(title || headerContent);
		return (
			<BottomSheet
				isOpen={isOpen}
				onClose={onClose}
				snapPoints={[0, 0.6, 1]}
				initialSnap={1}
				title={title}
				showCloseButton={showCloseButton}
				disableDefaultHeader={!title && !showCloseButton}
			>
				<div className={styles.bottomSheetContent}>
					{headerContent && <div className={styles.headerSlot}>{headerContent}</div>}
					<div className={clsx(styles.groupStack, hasHeader && styles.groupStackWithHeader)}>
						{groups.map((group, index) => (
							<MenuGroup key={index} group={group} isLast={index === groups.length - 1} />
						))}
					</div>
				</div>
			</BottomSheet>
		);
	},
);
