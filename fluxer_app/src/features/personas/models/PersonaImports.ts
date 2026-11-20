type Nullish<T> = T | null | undefined;

export interface PluralkitSystem {
	readonly tag: string;
	readonly members: Array<PluralkitSystemMember>;
}

export interface PluralkitSystemMember {
	readonly name: string;
	readonly display_name?: Nullish<string>;
	readonly color?: Nullish<string | number>;
	readonly pronouns?: Nullish<string>;
	readonly avatar_url?: Nullish<string>;
	readonly banner?: Nullish<string>;
	readonly description?: Nullish<string>;
	readonly bio?: Nullish<string>;
	readonly proxy_tags?: Array<{prefix?: Nullish<string>; suffix?: Nullish<string>}>;
}

export interface TupperboxExport {
	readonly tuppers: Array<TupperboxTupper>;
}

export interface TupperboxTupper {
	readonly name: string;
	readonly brackets: [string, string];
	readonly avatar_url: Nullish<string>;
	readonly avatar?: Nullish<string>;
	readonly avatar_ext?: Nullish<string>;
	readonly banner?: Nullish<string>;
	readonly description?: Nullish<string>;
	readonly nick?: Nullish<string>;
}

export interface FishingBucketExport {
	readonly proxies: Array<FishingBucketProxy>;
}

export interface FishingBucketProxy {
	readonly name: string;
	readonly description?: Nullish<string>;
	readonly avatar_url?: Nullish<string>;
	readonly triggers?: Array<string>;
	readonly nickname?: Nullish<string>;
	readonly pronouns?: Nullish<string>;
}
