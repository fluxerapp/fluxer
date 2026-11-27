// SPDX-License-Identifier: AGPL-3.0-or-later

const READING_BAND = 140;

interface Section {
	heading: HTMLElement;
	link: HTMLAnchorElement;
}

function navigationType(): string {
	const entry = performance.getEntriesByType('navigation')[0] as PerformanceNavigationTiming | undefined;
	return entry?.type ?? 'navigate';
}

export class FluxerStarlightTOC extends HTMLElement {
	private _current = this.querySelector<HTMLAnchorElement>('a[aria-current="true"]');
	private sections: Array<Section> = [];
	private settled = false;
	private frame = 0;
	private pinned: string | null = null;
	private pinnedY: number | null = null;

	protected set current(link: HTMLAnchorElement) {
		if (link === this._current) {
			return;
		}
		if (this._current) {
			this._current.removeAttribute('aria-current');
		}
		link.setAttribute('aria-current', 'true');
		this._current = link;
		this.keepInView(link);
	}

	constructor() {
		super();
		this.sections = this.collectSections();
		if (this.sections.length === 0) {
			return;
		}
		const restoring = navigationType() === 'reload' || navigationType() === 'back_forward';
		if (!restoring && location.hash.length > 1) {
			this.pin(location.hash);
		}
		const waiting =
			document.readyState !== 'complete' && window.scrollY === 0 && (restoring || location.hash.length > 1);
		if (!waiting) {
			this.update();
		}
		if (document.readyState !== 'complete') {
			addEventListener('load', () => this.update(), {once: true});
		}
		addEventListener('scroll', this.schedule, {passive: true});
		addEventListener('resize', this.schedule, {passive: true});
		addEventListener('hashchange', () => {
			if (this.pinned !== location.hash) {
				this.pin(location.hash);
			}
			this.schedule();
		});
		for (const {link} of this.sections) {
			link.addEventListener('click', () => {
				this.pin(link.hash);
				this.schedule();
			});
		}
	}

	private collectSections(): Array<Section> {
		const sections: Array<Section> = [];
		for (const link of this.querySelectorAll<HTMLAnchorElement>('a[href^="#"]')) {
			const heading = document.getElementById(decodeURIComponent(link.hash.slice(1)));
			if (heading) {
				sections.push({heading, link});
			}
		}
		return sections;
	}

	private pin(hash: string): void {
		this.pinned = hash;
		this.pinnedY = null;
	}

	private schedule = (): void => {
		if (this.frame !== 0) {
			return;
		}
		this.frame = requestAnimationFrame(() => {
			this.frame = 0;
			this.update();
		});
	};

	private update(): void {
		const link = this.pick();
		if (link) {
			this.current = link;
		}
		this.settled = true;
	}

	private pick(): HTMLAnchorElement | undefined {
		if (this.pinned !== null) {
			const pinned = this.sections.find((section) => section.link.hash === this.pinned);
			if (this.pinnedY === null) {
				this.pinnedY = window.scrollY;
			}
			if (pinned && Math.abs(window.scrollY - this.pinnedY) <= 2) {
				return pinned.link;
			}
			this.pinned = null;
			this.pinnedY = null;
		}
		const line = this.readingLine() + READING_BAND;
		let match = this.sections[0]?.link;
		for (const {heading, link} of this.sections) {
			if (heading.getBoundingClientRect().top > line) {
				break;
			}
			match = link;
		}
		return match;
	}

	private readingLine(): number {
		const navBarHeight = document.querySelector('header')?.getBoundingClientRect().height || 0;
		const mobileTocHeight = this.querySelector('summary')?.getBoundingClientRect().height || 0;
		return navBarHeight + mobileTocHeight + 32;
	}

	protected scroller(): HTMLElement | null {
		const scroller = this.closest<HTMLElement>('.right-sidebar');
		if (!scroller || scroller.clientHeight === 0) {
			return null;
		}
		return scroller;
	}

	private keepInView(link: HTMLAnchorElement): void {
		const scroller = this.scroller();
		if (!scroller) {
			return;
		}
		const box = scroller.getBoundingClientRect();
		const item = link.getBoundingClientRect();
		const margin = 48;
		let delta = 0;
		if (item.top < box.top + margin) {
			delta = item.top - (box.top + margin);
		} else if (item.bottom > box.bottom - margin) {
			delta = item.bottom - (box.bottom - margin);
		}
		if (delta === 0) {
			return;
		}
		const smooth = this.settled && !matchMedia('(prefers-reduced-motion: reduce)').matches;
		scroller.scrollBy({top: delta, behavior: smooth ? 'smooth' : 'instant'});
	}
}

if (!customElements.get('starlight-toc')) {
	customElements.define('starlight-toc', FluxerStarlightTOC);
}
