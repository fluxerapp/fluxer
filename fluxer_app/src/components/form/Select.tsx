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
import ReactSelect, {
	type ActionMeta,
	type ControlProps,
	type GroupBase,
	type InputProps,
	type MenuListProps,
	type MenuPlacement,
	type MultiValue,
	type OnChangeValue,
	type OptionProps,
	type Props as ReactSelectPropsConfig,
	components as reactSelectComponents,
} from 'react-select';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import {getSelectStyles} from '~/utils/SelectUtils';
import styles from './Select.module.css';
import {SelectBottomSheet} from './SelectBottomSheet';

type Primitive = string | number | null;

export type SelectOption<V extends Primitive = string> = {
	value: V;
	label: string;
	isDisabled?: boolean;
};

type SelectGroup<V extends Primitive> = GroupBase<SelectOption<V>>;
type ReactSelectProps<V extends Primitive, IsMulti extends boolean> = ReactSelectPropsConfig<
	SelectOption<V>,
	IsMulti,
	SelectGroup<V>
>;

interface SelectProps<
	V extends Primitive = string,
	IsMulti extends boolean = false,
	O extends SelectOption<V> = SelectOption<V>,
> {
	label?: string;
	description?: string;
	value: IsMulti extends true ? Array<V> : V;
	options: ReadonlyArray<O>;
	onChange: (value: IsMulti extends true ? Array<V> : V) => void;
	disabled?: boolean;
	error?: string;
	placeholder?: string;
	id?: string;
	className?: string;
	isSearchable?: boolean;
	tabIndex?: number;
	tabSelectsValue?: boolean;
	blurInputOnSelect?: boolean;
	openMenuOnFocus?: boolean;
	closeMenuOnSelect?: boolean;
	autoSelectExactMatch?: boolean;
	components?: ReactSelectProps<V, IsMulti>['components'];
	isLoading?: boolean;
	isClearable?: boolean;
	filterOption?: ReactSelectProps<V, IsMulti>['filterOption'];
	isMulti?: IsMulti;
	menuPlacement?: ReactSelectProps<V, IsMulti>['menuPlacement'];
	menuShouldScrollIntoView?: ReactSelectProps<V, IsMulti>['menuShouldScrollIntoView'];
	maxMenuHeight?: number;
	renderOption?: (option: O, isSelected: boolean) => React.ReactNode;
	renderValue?: (option: IsMulti extends true ? Array<O> : O | null) => React.ReactNode;
}

const SelectDesktop = observer(function SelectDesktop<
	V extends Primitive = string,
	IsMulti extends boolean = false,
	O extends SelectOption<V> = SelectOption<V>,
>({
	id,
	label,
	description,
	value,
	options,
	onChange,
	disabled = false,
	error,
	placeholder,
	className,
	isSearchable = true,
	tabIndex,
	tabSelectsValue = false,
	blurInputOnSelect = true,
	openMenuOnFocus = false,
	closeMenuOnSelect = true,
	autoSelectExactMatch = false,
	components: componentsProp,
	isLoading,
	isClearable,
	filterOption,
	isMulti,
	menuPlacement: menuPlacementProp,
	menuShouldScrollIntoView,
	maxMenuHeight: maxMenuHeightProp,
	renderOption,
	renderValue,
}: SelectProps<V, IsMulti, O>) {
	const generatedId = React.useId();
	const inputId = id ?? generatedId;
	const controlRef = React.useRef<HTMLDivElement | null>(null);
	const inputRef = React.useRef<HTMLInputElement | null>(null);
	const menuListRef = React.useRef<HTMLDivElement | null>(null);
	const [calculatedMenuPlacement, setCalculatedMenuPlacement] = React.useState<MenuPlacement>('auto');
	const [calculatedMaxHeight, setCalculatedMaxHeight] = React.useState<number>(300);

	const selectedOption = React.useMemo(() => {
		if (isMulti) {
			if (!Array.isArray(value)) return [];
			return options.filter((option) => (value as Array<V>).includes(option.value));
		}
		return options.find((option) => option.value === value) || null;
	}, [isMulti, options, value]);

	const handleChange = (
		newValue: OnChangeValue<SelectOption<V>, IsMulti>,
		_actionMeta: ActionMeta<SelectOption<V>>,
	) => {
		if (isMulti) {
			const vals = (newValue as MultiValue<SelectOption<V>>).map((o) => o.value);
			(onChange as (value: Array<V>) => void)(vals);
		} else {
			if (newValue) {
				(onChange as (value: V) => void)((newValue as SelectOption<V>).value);
			}
		}
	};

	const handleBlur = (event: React.FocusEvent<HTMLInputElement>) => {
		if (!autoSelectExactMatch || isMulti) return;

		const raw = event.target.value ?? '';
		const inputValue = raw.trim();
		if (!inputValue) return;

		const normalizeNumeric = (s: string) => {
			const m = s.match(/^\s*0*([0-9]+)\s*$/);
			return m ? m[1] : null;
		};

		const lowered = inputValue.toLowerCase();
		const numeric = normalizeNumeric(inputValue);

		const candidates = options.filter((option) => {
			const ovString = option.value == null ? '' : String(option.value);
			if (ovString === inputValue) return true;
			if (option.label.toLowerCase() === lowered) return true;
			if (numeric != null) {
				const ovNum = normalizeNumeric(ovString);
				if (ovNum != null && ovNum === numeric) return true;
			}
			return false;
		});

		if (candidates.length === 1) {
			(onChange as (value: V) => void)(candidates[0].value);
			return;
		}

		const defaultFilter = (label: string, input: string) => label.toLowerCase().includes(input.toLowerCase());

		const filteredOptions = options.filter((option) => {
			if (filterOption) {
				const filterOptionWrapper = {
					label: option.label,
					value: String(option.value),
					data: option,
				};
				return filterOption(filterOptionWrapper, inputValue);
			}
			return defaultFilter(option.label, inputValue);
		});

		if (filteredOptions.length > 0) {
			(onChange as (value: V) => void)(filteredOptions[0].value);
		}
	};

	const mergedComponents = React.useMemo(() => {
		const Control = (controlProps: ControlProps<SelectOption<V>, IsMulti>) => (
			<reactSelectComponents.Control
				{...controlProps}
				innerRef={(node) => {
					controlRef.current = node;
					const ref = controlProps.innerRef;
					if (typeof ref === 'function') ref(node);
					else if (ref) (ref as React.MutableRefObject<HTMLDivElement | null>).current = node;
				}}
			/>
		);

		const Input = (inputProps: InputProps<SelectOption<V>, IsMulti>) => (
			<reactSelectComponents.Input
				{...inputProps}
				innerRef={(node) => {
					inputRef.current = node;
					const ref = inputProps.innerRef;
					if (typeof ref === 'function') ref(node);
					else if (ref) (ref as React.MutableRefObject<HTMLInputElement | null>).current = node;
				}}
			/>
		);

		const MenuListComponent = componentsProp?.MenuList ?? reactSelectComponents.MenuList;
		const MenuList = (menuListProps: MenuListProps<SelectOption<V>, IsMulti, SelectGroup<V>>) => {
			const {innerRef, ...restProps} = menuListProps;
			return (
				<MenuListComponent
					{...restProps}
					innerRef={(node) => {
						menuListRef.current = node;
						if (typeof innerRef === 'function') {
							innerRef(node);
						} else if (innerRef) {
							(innerRef as React.MutableRefObject<HTMLDivElement | null>).current = node;
						}
					}}
				/>
			);
		};

		const OptionComponent = componentsProp?.Option ?? reactSelectComponents.Option;
		const Option = (optionProps: OptionProps<SelectOption<V>, IsMulti, SelectGroup<V>>) => {
			const {innerProps, ...restProps} = optionProps;
			const {onMouseMove: _onMouseMove, onMouseOver: _onMouseOver, ...innerPropsWithoutHover} = innerProps;
			return <OptionComponent {...restProps} innerProps={innerPropsWithoutHover} />;
		};

		return {
			...(componentsProp ?? {}),
			Control,
			Input,
			MenuList,
			Option,
		};
	}, [componentsProp]);

	const updateMenuPlacement = React.useCallback(() => {
		const controlNode = controlRef.current;
		if (!controlNode) return;

		const rect = controlNode.getBoundingClientRect();
		const viewportHeight = window.innerHeight || document.documentElement.clientHeight;
		const spaceAbove = rect.top;
		const spaceBelow = viewportHeight - rect.bottom;

		const prefersTop = spaceAbove > spaceBelow && spaceAbove > 200;
		const availableSpace = Math.max(prefersTop ? spaceAbove : spaceBelow, 180) - 12;

		setCalculatedMenuPlacement(prefersTop ? 'top' : 'bottom');
		setCalculatedMaxHeight(Math.max(180, Math.min(availableSpace, 300)));
	}, []);

	const handleDocumentScroll = React.useCallback(
		(event: Event) => {
			if (menuListRef.current && event.target instanceof Node && menuListRef.current.contains(event.target)) {
				return;
			}
			updateMenuPlacement();
		},
		[updateMenuPlacement],
	);

	React.useEffect(() => {
		updateMenuPlacement();
		const handleResize = () => updateMenuPlacement();
		window.addEventListener('resize', handleResize);
		document.addEventListener('scroll', handleDocumentScroll, true);
		return () => {
			window.removeEventListener('resize', handleResize);
			document.removeEventListener('scroll', handleDocumentScroll, true);
		};
	}, [updateMenuPlacement, handleDocumentScroll]);

	const handleMenuOpen = React.useCallback(() => {
		updateMenuPlacement();
	}, [updateMenuPlacement]);

	return (
		<div className={styles.container}>
			{label && (
				<label htmlFor={inputId} className={clsx(styles.label, disabled && styles.disabled)}>
					{label}
				</label>
			)}
			<div className={className}>
				<FocusRing focusTarget={inputRef} ringTarget={controlRef} offset={-2} enabled={!disabled} within={true}>
					<ReactSelect<SelectOption<V>, IsMulti>
						inputId={inputId}
						value={selectedOption as SelectOption<V> | Array<SelectOption<V>> | null}
						options={options}
						onChange={handleChange}
						isDisabled={disabled}
						placeholder={placeholder}
						styles={getSelectStyles<SelectOption<V>, IsMulti>(!!error)}
						isSearchable={isSearchable}
						tabIndex={tabIndex}
						tabSelectsValue={tabSelectsValue}
						blurInputOnSelect={blurInputOnSelect}
						openMenuOnFocus={openMenuOnFocus}
						closeMenuOnSelect={closeMenuOnSelect}
						menuPortalTarget={document.body}
						menuPosition="fixed"
						menuPlacement={menuPlacementProp ?? calculatedMenuPlacement}
						menuShouldScrollIntoView={menuShouldScrollIntoView ?? false}
						maxMenuHeight={maxMenuHeightProp ?? calculatedMaxHeight}
						components={mergedComponents}
						isLoading={isLoading}
						isClearable={isClearable}
						filterOption={filterOption}
						onBlur={handleBlur}
						isMulti={isMulti}
						onMenuOpen={handleMenuOpen}
						formatOptionLabel={(option, {context}) => {
							const isSelected = Array.isArray(selectedOption)
								? selectedOption.some((o) => o.value === option.value)
								: selectedOption?.value === option.value;
							if (context === 'value' && renderValue && !isMulti) {
								return renderValue(option as IsMulti extends true ? Array<O> : O | null);
							}
							if (renderOption) {
								return renderOption(option as O, isSelected);
							}
							return option.label;
						}}
					/>
				</FocusRing>
			</div>
			{description && <p className={clsx(styles.description, disabled && styles.disabled)}>{description}</p>}
			{error && <span className={styles.errorText}>{error}</span>}
		</div>
	);
});

export const Select = observer(function Select<
	V extends Primitive = string,
	IsMulti extends boolean = false,
	O extends SelectOption<V> = SelectOption<V>,
>({id, ...props}: SelectProps<V, IsMulti, O>) {
	const isMobileLayout = MobileLayoutStore.isMobileLayout();

	if (isMobileLayout) {
		return (
			<SelectBottomSheet
				id={id}
				label={props.label}
				description={props.description}
				value={props.value}
				options={props.options}
				onChange={props.onChange}
				disabled={props.disabled}
				error={props.error}
				placeholder={props.placeholder}
				className={props.className}
				isMulti={props.isMulti}
				renderOption={props.renderOption}
				renderValue={props.renderValue}
			/>
		);
	}

	return <SelectDesktop id={id} {...props} />;
});
