// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/ui/card_alignment_controls/CardAlignmentControls.module.css';
import type {TooltipPosition} from '@app/features/ui/tooltip/Tooltip';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import {GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {IconProps} from '@phosphor-icons/react';
import {TextAlignCenterIcon, TextAlignLeftIcon, TextAlignRightIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {useMemo} from 'react';

const LEFT_DESCRIPTOR = msg({
	message: 'Left',
	comment: 'Card alignment option that aligns content to the left.',
});
const CENTER_DESCRIPTOR = msg({
	message: 'Center',
	comment: 'Card alignment option that centers content.',
});
const RIGHT_DESCRIPTOR = msg({
	message: 'Right',
	comment: 'Card alignment option that aligns content to the right.',
});
const CARD_ALIGNMENT_CONTROLS_DESCRIPTOR = msg({
	message: 'Card alignment controls',
	comment: 'Accessible label for the card alignment radio group.',
});

interface CardAlignmentControlOption {
	value: GuildSplashCardAlignmentValue;
	label: MessageDescriptor;
	icon?: React.ComponentType<IconProps>;
}

interface CardAlignmentControlsProps {
	value: GuildSplashCardAlignmentValue;
	onChange: (alignment: GuildSplashCardAlignmentValue) => void;
	options?: ReadonlyArray<CardAlignmentControlOption>;
	disabled?: boolean;
	disabledTooltipText?: string;
	tooltipPosition?: TooltipPosition;
	className?: string;
}

const DEFAULT_ALIGNMENT_OPTIONS: ReadonlyArray<CardAlignmentControlOption> = [
	{value: GuildSplashCardAlignment.LEFT, label: LEFT_DESCRIPTOR, icon: TextAlignLeftIcon},
	{value: GuildSplashCardAlignment.CENTER, label: CENTER_DESCRIPTOR, icon: TextAlignCenterIcon},
	{value: GuildSplashCardAlignment.RIGHT, label: RIGHT_DESCRIPTOR, icon: TextAlignRightIcon},
];
export const CardAlignmentControls: React.FC<CardAlignmentControlsProps> = ({
	value,
	onChange,
	options = DEFAULT_ALIGNMENT_OPTIONS,
	disabled = false,
	disabledTooltipText,
	tooltipPosition = 'top',
	className,
}) => {
	const {i18n} = useLingui();
	const translatedOptions = useMemo(
		() => options.map((option) => ({...option, label: i18n._(option.label)})),
		[options, i18n.locale],
	);
	const controls = (
		<div
			className={clsx(styles.controls, disabled && styles.controlsDisabled, className)}
			role="group"
			aria-label={i18n._(CARD_ALIGNMENT_CONTROLS_DESCRIPTOR)}
			data-flx="ui.card-alignment-controls.card-alignment-controls.controls"
		>
			{translatedOptions.map((option) => {
				const isActive = value === option.value;
				const Icon = option.icon;
				const handleClick = () => {
					if (disabled) return;
					onChange(option.value);
				};
				const button = (
					<button
						type="button"
						className={clsx(styles.button, isActive && styles.buttonActive, disabled && styles.buttonDisabled)}
						onClick={handleClick}
						disabled={disabled}
						aria-pressed={isActive}
						aria-label={option.label}
						data-flx="ui.card-alignment-controls.card-alignment-controls.button.click"
					>
						{Icon ? (
							<Icon
								size={remFromPx(18)}
								weight={isActive ? 'bold' : 'regular'}
								data-flx="ui.card-alignment-controls.card-alignment-controls.icon"
							/>
						) : (
							option.label
						)}
					</button>
				);
				return (
					<Tooltip
						key={option.value}
						text={option.label}
						position="top"
						data-flx="ui.card-alignment-controls.card-alignment-controls.tooltip"
					>
						{button}
					</Tooltip>
				);
			})}
		</div>
	);
	if (disabled && disabledTooltipText) {
		return (
			<Tooltip
				text={disabledTooltipText}
				position={tooltipPosition}
				data-flx="ui.card-alignment-controls.card-alignment-controls.tooltip--2"
			>
				{controls}
			</Tooltip>
		);
	}
	return controls;
};
