// SPDX-License-Identifier: AGPL-3.0-or-later

import {getMuteDurationOptions} from '@app/features/channel/components/MuteOptions';
import {SubMenu} from '@app/features/ui/action_menu/ContextMenu';
import {MuteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {useLingui} from '@lingui/react/macro';

interface MuteTargetMenuItemProps {
	showUnmute: boolean;
	mutedText?: string | null;
	muteLabel: string;
	unmuteLabel: string;
	onMute(duration: number | null): void;
	onUnmute(): void;
	dataFlx: string;
}
export function MuteTargetMenuItem({
	showUnmute,
	mutedText,
	muteLabel,
	unmuteLabel,
	onMute,
	onUnmute,
	dataFlx,
}: MuteTargetMenuItemProps) {
	const {i18n} = useLingui();
	if (showUnmute)
		return (
			<MenuItem
				icon={<MuteIcon data-flx={`${dataFlx}.mute-icon`} />}
				onClick={onUnmute}
				hint={mutedText ?? undefined}
				data-flx={`${dataFlx}.menu-item.unmute`}
			>
				{unmuteLabel}
			</MenuItem>
		);
	return (
		<SubMenu
			label={muteLabel}
			onTriggerSelect={() => onMute(null)}
			render={() => (
				<MenuGroup data-flx={`${dataFlx}.menu-group`}>
					{getMuteDurationOptions(i18n).map((option) => (
						<MenuItem
							key={option.value ?? 'until'}
							onClick={() => onMute(option.value)}
							data-flx={`${dataFlx}.menu-item.mute`}
						>
							{option.label}
						</MenuItem>
					))}
				</MenuGroup>
			)}
			data-flx={`${dataFlx}.menu-item-submenu`}
		/>
	);
}
