// SPDX-License-Identifier: AGPL-3.0-or-later

import {Component, createRef, type HTMLAttributes} from 'react';

export function guildRailTransition(removing: boolean): KeyframeAnimationOptions {
	return {
		duration: removing ? 200 : 260,
		easing: removing ? 'cubic-bezier(0.645, 0.045, 0.355, 1)' : 'cubic-bezier(0.165, 0.84, 0.44, 1)',
		fill: 'both',
	};
}

export function animateGuildRailOffset(node: HTMLElement, offset: number, removing: boolean): Animation | null {
	if (!Number.isFinite(offset) || Math.abs(offset) < 0.01 || typeof node.animate !== 'function') return null;
	return node.animate([{translate: `0px ${offset}px`}, {translate: '0px 0px'}], guildRailTransition(removing));
}

export function guildRailDMClosedTransform(rtl: boolean): string {
	return `translateX(${rtl ? 88 : -88}px)`;
}

export function animateGuildRailDM(
	node: HTMLElement,
	removing: boolean,
	from: Keyframe | null,
	rtl: boolean,
): Animation | null {
	if (typeof node.animate !== 'function') return null;
	const closed = {transform: guildRailDMClosedTransform(rtl)};
	const open = {transform: 'translateX(0px)'};
	return node.animate([from ?? (removing ? open : closed), removing ? closed : open], guildRailTransition(removing));
}

interface GuildRailLayoutItemProps extends HTMLAttributes<HTMLDivElement> {
	layoutRevision: string;
	paused: boolean;
	reducedMotion: boolean;
	removing: boolean;
}

export class GuildRailLayoutItem extends Component<GuildRailLayoutItemProps, unknown, number | null> {
	private node = createRef<HTMLDivElement>();
	private animation: Animation | null = null;

	override getSnapshotBeforeUpdate(previous: GuildRailLayoutItemProps): number | null {
		if (previous.layoutRevision === this.props.layoutRevision || this.props.reducedMotion) return null;
		return this.node.current?.getBoundingClientRect().top ?? null;
	}

	override componentDidUpdate(previous: GuildRailLayoutItemProps, _state: unknown, top: number | null): void {
		if (this.props.reducedMotion) {
			this.animation?.cancel();
			this.animation = null;
			return;
		}
		const node = this.node.current;
		if (top !== null && node !== null) {
			this.animation?.cancel();
			this.animation = animateGuildRailOffset(node, top - node.getBoundingClientRect().top, this.props.removing);
			const animation = this.animation;
			void animation?.finished
				.then(() => {
					if (this.animation !== animation) return;
					this.animation = null;
					animation.cancel();
				})
				.catch(() => {});
		}
		if (this.props.paused) {
			if (this.animation?.playState === 'running') this.animation.pause();
		} else if (previous.paused && this.animation?.playState === 'paused') {
			this.animation.play();
		}
	}

	override componentWillUnmount(): void {
		this.animation?.cancel();
	}

	override render() {
		const {
			layoutRevision: _revision,
			paused: _paused,
			reducedMotion: _motion,
			removing: _removing,
			...props
		} = this.props;
		return <div ref={this.node} data-flx="app.guilds-layout.guild-rail-layout-animation.div" {...props} />;
	}
}
