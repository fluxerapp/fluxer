// SPDX-License-Identifier: AGPL-3.0-or-later

import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import styles from '@app/features/channel/components/ChannelHeader.module.css';
import type {KeybindCommand} from '@app/features/input/state/InputKeybind';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {TooltipWithKeybind} from '@app/features/ui/keybind_hint/KeybindHint';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {Icon, IconWeight} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import React, {useCallback, useRef} from 'react';

interface ChannelHeaderIconProps extends Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'children'> {
	icon: Icon;
	iconWeight?: IconWeight;
	label: string;
	isSelected?: boolean;
	onClick?: React.MouseEventHandler<HTMLButtonElement>;
	onContextMenu?: React.MouseEventHandler<HTMLButtonElement>;
	disabled?: boolean;
	keybindAction?: KeybindCommand;
}

export const ChannelHeaderIcon = React.forwardRef<HTMLButtonElement, ChannelHeaderIconProps>((props, ref) => {
	const {
		icon: Icon,
		iconWeight,
		label,
		isSelected = false,
		onClick,
		disabled = false,
		keybindAction,
		className,
		...rest
	} = props;
	const buttonRef = useRef<HTMLButtonElement | null>(null);
	const mergedRef = useMergeRefs([ref, buttonRef]);
	const hasPopup = rest['aria-haspopup'] !== undefined;
	const ariaPressed = rest['aria-pressed'] ?? (!hasPopup && isSelected ? true : undefined);
	const tooltipText = useCallback(
		() => (
			<TooltipWithKeybind
				label={label}
				action={keybindAction}
				data-flx="channel.channel-header-components.channel-header-icon.tooltip-text.tooltip-with-keybind"
			/>
		),
		[label, keybindAction],
	);
	const button = (
		<FocusRing
			offset={-2}
			enabled={!disabled}
			data-flx="channel.channel-header-components.channel-header-icon.focus-ring"
		>
			<button
				data-flx="channel.channel-header-components.channel-header-icon.icon-button.undefined"
				{...rest}
				ref={mergedRef}
				type="button"
				className={clsx(isSelected ? styles.iconButtonSelected : styles.iconButtonDefault, className)}
				aria-label={label}
				aria-pressed={ariaPressed}
				onClick={disabled ? undefined : onClick}
				disabled={disabled}
				style={{opacity: disabled ? 0.5 : 1, cursor: disabled ? 'not-allowed' : 'pointer'}}
			>
				<Icon
					weight={iconWeight}
					className={styles.buttonIcon}
					data-flx="channel.channel-header-components.channel-header-icon.button-icon"
				/>
			</button>
		</FocusRing>
	);
	if (disabled) {
		return (
			<Tooltip
				text={tooltipText}
				position="bottom"
				data-flx="channel.channel-header-components.channel-header-icon.tooltip"
			>
				<div style={{display: 'inline-flex'}} data-flx="channel.channel-header-components.channel-header-icon.div">
					{button}
				</div>
			</Tooltip>
		);
	}
	return (
		<Tooltip
			text={tooltipText}
			position="bottom"
			data-flx="channel.channel-header-components.channel-header-icon.tooltip--2"
		>
			{button}
		</Tooltip>
	);
});

ChannelHeaderIcon.displayName = 'ChannelHeaderIcon';
