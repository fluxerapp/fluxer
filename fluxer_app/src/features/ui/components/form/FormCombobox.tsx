// SPDX-License-Identifier: AGPL-3.0-or-later

import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import styles from '@app/features/ui/components/form/FormCombobox.module.css';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import {Combobox as BaseCombobox} from '@base-ui/react/combobox';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, CheckIcon, CircleNotchIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useId, useMemo, useRef, useState} from 'react';

type Primitive = string | number | null;
export interface ComboboxOption<V extends Primitive = string> {
	value: V;
	label: string;
	isDisabled?: boolean;
}

function findOptionByLabel<V extends Primitive, O extends ComboboxOption<V>>(
	inputValue: string,
	options: ReadonlyArray<O>,
): O | undefined {
	const lowered = inputValue.trim().toLowerCase();
	return options.find((option) => option.label.trim().toLowerCase() === lowered);
}

export interface ComboboxFilterOption<O> {
	label: string;
	value: string;
	data: O;
}

type ComboboxInputValueResolver<V extends Primitive, O extends ComboboxOption<V>> = (
	inputValue: string,
	options: ReadonlyArray<O>,
) => V | undefined;

export interface ComboboxProps<V extends Primitive = string, O extends ComboboxOption<V> = ComboboxOption<V>> {
	label?: React.ReactNode;
	description?: string;
	value: V;
	options: ReadonlyArray<O>;
	onChange: (value: V) => void;
	disabled?: boolean;
	error?: string;
	placeholder?: string;
	id?: string;
	className?: string;
	isSearchable?: boolean;
	tabIndex?: number;
	blurInputOnSelect?: boolean;
	openMenuOnFocus?: boolean;
	closeMenuOnSelect?: boolean;
	autoSelectExactMatch?: boolean;
	autoSelectValueFromInput?: ComboboxInputValueResolver<V, O>;
	isLoading?: boolean;
	isClearable?: boolean;
	filterOption?: (option: ComboboxFilterOption<O>, inputValue: string) => boolean;
	menuPlacement?: 'auto' | 'bottom' | 'top';
	maxMenuHeight?: number | null;
	menuMinWidth?: number;
	wrapMenuText?: boolean;
	renderOption?: (option: O, isSelected: boolean) => React.ReactNode;
	renderValue?: (option: O | null) => React.ReactNode;
	density?: 'default' | 'compact' | 'compactOverlay';
	dir?: 'ltr' | 'rtl';
	'aria-label'?: string;
	'data-flx'?: string;
}

const SELECT_DESCRIPTOR = msg({
	message: 'Select…',
	comment: 'Generic combobox control placeholder.',
});
const NO_RESULTS_FOUND_DESCRIPTOR = msg({
	message: 'No results found',
	comment: 'Empty state shown when a combobox search returns no matches.',
});
const LOADING_DESCRIPTOR = msg({
	message: 'Loading…',
	comment: 'Combobox popup status shown while options are loading.',
});
const CLEAR_SELECTION_DESCRIPTOR = msg({
	message: 'Clear selection',
	comment: 'Accessible label for the combobox clear button.',
});
const OPEN_POPUP_DESCRIPTOR = msg({
	message: 'Open popup',
	comment: 'Accessible label for the combobox popup trigger button.',
});

const COMBOBOX_DEFAULT_MAX_HEIGHT = 300;
const COMBOBOX_SCROLL_ITEM_THRESHOLD = 15;

const densityInputGroupClass: Record<NonNullable<ComboboxProps['density']>, string> = {
	default: styles.inputGroupDefault,
	compact: styles.inputGroupCompact,
	compactOverlay: styles.inputGroupCompactOverlay,
};

const densityInputClass: Record<NonNullable<ComboboxProps['density']>, string> = {
	default: styles.inputDefault,
	compact: styles.inputCompact,
	compactOverlay: styles.inputCompactOverlay,
};

const densityValueOverlayClass: Record<NonNullable<ComboboxProps['density']>, string> = {
	default: styles.valueOverlayDefault,
	compact: styles.valueOverlayCompact,
	compactOverlay: styles.valueOverlayCompactOverlay,
};

const densityPopupClass: Record<NonNullable<ComboboxProps['density']>, string> = {
	default: styles.popupDefault,
	compact: styles.popupCompact,
	compactOverlay: styles.popupCompactOverlay,
};

function stringifyOptionValue<V extends Primitive>(value: V): string {
	return value == null ? '' : String(value);
}

function normalizeNumeric(value: string): string | null {
	const match = value.match(/^\s*0*([0-9]+)\s*$/);
	return match ? match[1] : null;
}

export const Combobox = observer(function Combobox<
	V extends Primitive = string,
	O extends ComboboxOption<V> = ComboboxOption<V>,
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
	blurInputOnSelect = true,
	openMenuOnFocus = false,
	closeMenuOnSelect = true,
	autoSelectExactMatch = false,
	autoSelectValueFromInput,
	isLoading = false,
	isClearable = false,
	filterOption,
	menuPlacement: menuPlacementProp = 'auto',
	maxMenuHeight: maxMenuHeightProp,
	menuMinWidth,
	wrapMenuText,
	renderOption,
	renderValue,
	density = 'default',
	dir,
	'aria-label': ariaLabel,
	'data-flx': dataFlx,
}: ComboboxProps<V, O>) {
	const {i18n} = useLingui();
	const portalHost = usePortalHost();
	const generatedId = useId();
	const inputId = id ?? generatedId;
	const inputRef = useRef<HTMLInputElement | null>(null);
	const controlRef = useRef<HTMLDivElement | null>(null);
	const [open, setOpen] = useState(false);
	const [isTyping, setIsTyping] = useState(false);
	const selectedOptions = useMemo(() => {
		return options.find((option) => Object.is(option.value, value as V)) ?? null;
	}, [options, value]);
	const selectedOption = selectedOptions;
	const comboboxValue = selectedOptions as O | null;
	const popupMaxHeight = useMemo(() => {
		if (maxMenuHeightProp === null || options.length < COMBOBOX_SCROLL_ITEM_THRESHOLD) return 'var(--available-height)';
		return `min(var(--available-height), ${maxMenuHeightProp ?? COMBOBOX_DEFAULT_MAX_HEIGHT}px)`;
	}, [maxMenuHeightProp, options.length]);
	const popupMinWidth = menuMinWidth == null ? 'var(--anchor-width)' : `max(var(--anchor-width), ${menuMinWidth}px)`;
	const popupStyle = useMemo<React.CSSProperties>(
		() => ({
			maxHeight: popupMaxHeight,
			minWidth: popupMinWidth,
		}),
		[popupMaxHeight, popupMinWidth],
	);
	const resolveInputValue = useCallback(
		(inputValue: string): V | undefined => {
			const labelMatch = findOptionByLabel<V, O>(inputValue, options);
			if (labelMatch) return labelMatch.value;
			const customValue = autoSelectValueFromInput?.(inputValue, options);
			if (customValue !== undefined) return customValue;
			if (!autoSelectExactMatch) return undefined;
			const lowered = inputValue.toLowerCase();
			const numeric = normalizeNumeric(inputValue);
			const candidates = options.filter((option) => {
				const optionValue = stringifyOptionValue(option.value);
				if (optionValue === inputValue) return true;
				if (option.label.toLowerCase() === lowered) return true;
				if (numeric != null) {
					const optionNumeric = normalizeNumeric(optionValue);
					if (optionNumeric != null && optionNumeric === numeric) return true;
				}
				return false;
			});
			if (candidates.length === 1) return candidates[0].value;
			const filteredOptions = options.filter((option) => {
				if (filterOption) {
					return filterOption(
						{
							label: option.label,
							value: stringifyOptionValue(option.value),
							data: option,
						},
						inputValue,
					);
				}
				return option.label.toLowerCase().includes(inputValue.toLowerCase());
			});
			return filteredOptions[0]?.value;
		},
		[autoSelectExactMatch, autoSelectValueFromInput, filterOption, options],
	);
	const handleBlur = useCallback(
		(event: React.FocusEvent<HTMLInputElement>) => {
			if (!autoSelectExactMatch && !autoSelectValueFromInput) return;
			const inputValue = (event.target.value ?? '').trim();
			if (!inputValue) return;
			const resolvedValue = resolveInputValue(inputValue);
			if (resolvedValue !== undefined) (onChange as (value: V) => void)(resolvedValue);
		},
		[autoSelectExactMatch, autoSelectValueFromInput, onChange, resolveInputValue],
	);
	const selectSelectedLabel = useCallback(() => {
		if (!isSearchable || isTyping) return;
		inputRef.current?.select();
	}, [isSearchable, isTyping]);
	const handleFocus = useCallback(() => {
		if (openMenuOnFocus && !disabled) setOpen(true);
		selectSelectedLabel();
	}, [disabled, openMenuOnFocus, selectSelectedLabel]);
	const handleInputValueChange = useCallback(
		(_inputValue: string, eventDetails: BaseCombobox.Root.ChangeEventDetails) => {
			if (eventDetails.reason === 'input-change') setIsTyping(true);
		},
		[],
	);
	const handleInputPointerDown = useCallback(
		(event: React.PointerEvent<HTMLInputElement>) => {
			if (isSearchable || disabled) return;
			event.preventDefault();
			inputRef.current?.focus();
			setOpen(!open);
		},
		[disabled, isSearchable, open],
	);
	const handleOpenChange = useCallback(
		(nextOpen: boolean, eventDetails: BaseCombobox.Root.ChangeEventDetails) => {
			if (!nextOpen && !closeMenuOnSelect && eventDetails.reason === 'item-press') {
				eventDetails.cancel();
				return;
			}
			if (!nextOpen) setIsTyping(false);
			setOpen(nextOpen);
		},
		[closeMenuOnSelect],
	);
	const handleValueChange = useCallback(
		(nextValue: O | null, eventDetails: BaseCombobox.Root.ChangeEventDetails) => {
			if (nextValue == null) {
				if (isClearable) (onChange as (value: V) => void)(null as V);
			} else {
				(onChange as (value: V) => void)((nextValue as O).value);
			}
			if (blurInputOnSelect && eventDetails.reason === 'item-press') {
				window.requestAnimationFrame(() => inputRef.current?.blur());
			}
		},
		[blurInputOnSelect, isClearable, onChange],
	);
	const comboboxFilter = useCallback(
		(option: O, query: string) => {
			if (!filterOption) return option.label.toLowerCase().includes(query.toLowerCase());
			return filterOption(
				{
					label: option.label,
					value: stringifyOptionValue(option.value),
					data: option,
				},
				query,
			);
		},
		[filterOption],
	);
	const renderedValue = useMemo(() => {
		if (!selectedOption) return null;
		if (renderValue) return renderValue(selectedOption as O | null);
		return null;
	}, [i18n.locale, renderValue, selectedOption]);
	const shouldShowValueOverlay =
		Boolean(renderedValue) && (!open || !isSearchable || (Boolean(renderValue) && !isTyping));
	const hasSelectedValue = selectedOption != null;
	const emptyMessage = isLoading ? i18n._(LOADING_DESCRIPTOR) : i18n._(NO_RESULTS_FOUND_DESCRIPTOR);
	return (
		<div className={styles.container} data-flx={dataFlx ?? 'ui.form.combobox.container'}>
			{label && (
				<label
					htmlFor={inputId}
					className={clsx(styles.label, disabled && styles.disabled)}
					data-flx="ui.form.combobox.label"
				>
					{label}
				</label>
			)}
			<div className={className} data-flx="ui.form.combobox.control-wrap">
				<BaseCombobox.Root<O>
					id={inputId}
					items={options}
					value={comboboxValue}
					onValueChange={handleValueChange}
					open={open}
					onOpenChange={handleOpenChange}
					onInputValueChange={handleInputValueChange}
					disabled={disabled}
					filter={isSearchable ? comboboxFilter : null}
					autoHighlight={true}
					openOnInputClick={isSearchable}
					itemToStringLabel={(option) => option.label}
					itemToStringValue={(option) => stringifyOptionValue(option.value)}
					isItemEqualToValue={(option, selected) => Object.is(option.value, selected.value)}
					autoComplete={isSearchable ? 'list' : 'none'}
					data-flx="ui.form.form-combobox.combobox.base-combobox-root"
				>
					<FocusRing
						focusTarget={inputRef}
						ringTarget={controlRef}
						offset={-2}
						enabled={!disabled}
						within={true}
						data-flx="ui.form.combobox.focus-ring"
					>
						<BaseCombobox.InputGroup
							ref={controlRef}
							className={(state) =>
								clsx(
									styles.inputGroup,
									densityInputGroupClass[density],
									state.open && styles.inputGroupOpen,
									state.disabled && styles.inputGroupDisabled,
									error && styles.inputGroupError,
								)
							}
							data-flx="ui.form.combobox.input-group"
						>
							<BaseCombobox.Input
								ref={inputRef}
								id={inputId}
								placeholder={placeholder ?? i18n._(SELECT_DESCRIPTOR)}
								className={clsx(
									styles.input,
									densityInputClass[density],
									shouldShowValueOverlay && styles.inputWithValueOverlay,
								)}
								disabled={disabled}
								readOnly={!isSearchable}
								tabIndex={tabIndex}
								onBlur={handleBlur}
								onFocus={handleFocus}
								onClick={selectSelectedLabel}
								onPointerDown={handleInputPointerDown}
								aria-label={ariaLabel}
								{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
								data-flx="ui.form.combobox.input"
							/>
							{shouldShowValueOverlay && (
								<div
									className={clsx(
										styles.valueOverlay,
										densityValueOverlayClass[density],
										isClearable && hasSelectedValue && styles.valueOverlayWithClear,
									)}
									aria-hidden
									data-flx="ui.form.combobox.value-overlay"
								>
									{renderedValue}
								</div>
							)}
							<div className={styles.actionButtons} data-flx="ui.form.combobox.action-buttons">
								{isClearable && (
									<BaseCombobox.Clear
										className={styles.clearButton}
										disabled={disabled}
										aria-label={i18n._(CLEAR_SELECTION_DESCRIPTOR)}
										data-flx="ui.form.combobox.clear-button"
									>
										<XIcon weight="bold" data-flx="ui.form.combobox.clear-icon" />
									</BaseCombobox.Clear>
								)}
								<BaseCombobox.Trigger
									className={(state) => clsx(styles.triggerButton, state.open && styles.triggerButtonOpen)}
									disabled={disabled}
									aria-label={i18n._(OPEN_POPUP_DESCRIPTOR)}
									data-flx="ui.form.combobox.trigger-button"
								>
									{isLoading ? (
										<CircleNotchIcon
											weight="bold"
											className={styles.loadingIcon}
											data-flx="ui.form.combobox.loading-icon"
										/>
									) : (
										<CaretDownIcon
											weight="bold"
											className={styles.triggerIcon}
											data-flx="ui.form.combobox.trigger-icon"
										/>
									)}
								</BaseCombobox.Trigger>
							</div>
						</BaseCombobox.InputGroup>
					</FocusRing>
					<BaseCombobox.Portal container={portalHost ?? undefined} data-flx="ui.form.combobox.portal">
						<BaseCombobox.Positioner
							className={styles.positioner}
							positionMethod="fixed"
							side={menuPlacementProp === 'top' ? 'top' : 'bottom'}
							align="start"
							sideOffset={4}
							collisionPadding={12}
							collisionAvoidance={{
								side: menuPlacementProp === 'auto' ? 'flip' : 'shift',
								align: 'shift',
								fallbackAxisSide: 'none',
							}}
							dir={dir}
							data-flx="ui.form.combobox.positioner"
						>
							<BaseCombobox.Popup
								className={clsx(styles.popup, densityPopupClass[density])}
								style={popupStyle}
								data-flx="ui.form.combobox.popup"
							>
								<BaseCombobox.Empty className={styles.emptyState} data-flx="ui.form.combobox.empty-state">
									{emptyMessage}
								</BaseCombobox.Empty>
								<Scroller
									className={styles.listScroller}
									contentClassName={styles.listScrollerContent}
									overflow="auto"
									fade={false}
									scrollbarTrackMode="overlay"
									dir={dir}
									data-flx="ui.form.combobox.list-scroller"
								>
									<BaseCombobox.List className={styles.list} data-flx="ui.form.combobox.list">
										{(option: O, index: number) => (
											<BaseCombobox.Item
												key={`${stringifyOptionValue(option.value)}-${index}`}
												value={option}
												disabled={option.isDisabled}
												className={(state) =>
													clsx(
														styles.item,
														renderOption && styles.itemRendered,
														state.highlighted && styles.itemHighlighted,
														state.selected && styles.itemSelected,
														state.disabled && styles.itemDisabled,
														wrapMenuText && styles.itemWrap,
													)
												}
												data-flx="ui.form.combobox.item"
											>
												<BaseCombobox.ItemIndicator
													keepMounted
													className={(state) =>
														clsx(styles.itemIndicator, !state.selected && styles.itemIndicatorHidden)
													}
													data-flx="ui.form.combobox.item-indicator"
												>
													<CheckIcon weight="bold" data-flx="ui.form.combobox.item-check-icon" />
												</BaseCombobox.ItemIndicator>
												<span className={styles.itemText} data-flx="ui.form.combobox.item-text">
													{renderOption?.(option, selectedOption === option) ?? option.label}
												</span>
											</BaseCombobox.Item>
										)}
									</BaseCombobox.List>
								</Scroller>
							</BaseCombobox.Popup>
						</BaseCombobox.Positioner>
					</BaseCombobox.Portal>
				</BaseCombobox.Root>
			</div>
			{description && (
				<p className={clsx(styles.description, disabled && styles.disabled)} data-flx="ui.form.combobox.description">
					{description}
				</p>
			)}
			{error && (
				<span className={styles.errorText} data-flx="ui.form.combobox.error-text">
					{error}
				</span>
			)}
		</div>
	);
});
