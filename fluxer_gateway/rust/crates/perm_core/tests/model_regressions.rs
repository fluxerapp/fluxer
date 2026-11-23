// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Config, Event, Hoist, Meter, Nsfw,
    Permissions, PermsShared, RankPos, RoleId, RoleList, RoleSpec, Snapshot, UserId,
};
use support::{
    Driver, GUILD, Op, Params, apply_step, check_effects, check_view, guild, maybe_compact,
};

fn replay(params: Params, steps: Vec<Vec<Op>>) {
    let mut driver = Driver::new(&params);
    check_view(&driver);
    for ops in &steps {
        let (before, effects) = apply_step(&mut driver, ops);
        check_effects(&driver, &before, &effects);
        check_view(&driver);
        if maybe_compact(&mut driver, &effects).is_some() {
            check_view(&driver);
        }
    }
}

fn channel(id: u64, kind: ChannelKind, parent: Option<u64>, nsfw: Nsfw) -> ChannelSpec {
    ChannelSpec {
        id: ChannelId::new(id),
        kind,
        parent: parent.map(ChannelId::new),
        overwrites: vec![],
        nsfw,
        named: true,
    }
}

#[test]
fn a_rename_keeps_the_parent_for_nsfw_inheritance() {
    let mut s = Snapshot::new(guild(), None);
    s.roles = vec![RoleSpec {
        id: RoleId::new(GUILD),
        permissions: Permissions::VIEW_CHANNEL,
        position: RankPos::Int(0),
        hoist: Hoist::NotHoisted,
    }];
    s.channels = vec![
        channel(1, ChannelKind::Category, None, Nsfw::Adult),
        channel(2, ChannelKind::Text, Some(1), Nsfw::Inherit),
    ];
    s.members.push((UserId::new(1), RoleList::Roles(vec![])));
    let shared = PermsShared::new(guild(), Config::default());
    shared.load(&s).unwrap();
    let mut renamed = channel(2, ChannelKind::Text, Some(1), Nsfw::Inherit);
    renamed.named = false;
    shared
        .apply(&[Event::ChannelUpsert(renamed, ChannelCause::Updated)])
        .unwrap();
    let view = shared.view().unwrap();
    let mut meter = Meter::new();
    assert_eq!(view.visitor_channels(false, &mut meter), vec![]);
    assert_eq!(view.visitor_channels(true, &mut meter).len(), 2);
}

#[test]
fn shrunk_visitors_after_a_channel_edit() {
    replay(
        Params {
            roles: 11,
            channels: 21,
            members: 2,
            seed: 512_503_666_880_079_818,
            budget: 250_000,
            floor: 4096,
            cache: 1_048_576,
        },
        vec![
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelOrder(9_644_225_307_260_552_037)],
            vec![Op::ChannelEdit(20423, 2_589_434_858_466_556_433)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(330)],
            vec![
                Op::RoleEdit(82, 16_956_497_392_477_383_471),
                Op::VirtualAccess(46627, vec![4709]),
                Op::MemberRemove(60204),
            ],
        ],
    );
}

#[test]
fn shrunk_wip_seed_2() {
    replay(
        Params {
            roles: 2,
            channels: 20,
            members: 1,
            seed: 4_288_654_482_926_974_043,
            budget: 250_000,
            floor: 4096,
            cache: 1_048_576,
        },
        vec![
            vec![Op::ChannelDelete(6911)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelDelete(60)],
            vec![Op::ChannelOrder(12_341_554_863_647_413_412)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelDelete(3480)],
            vec![Op::ChannelCreate(0)],
            vec![
                Op::MemberUpsert(0, 9_785_696_878_254_141_802),
                Op::ChannelCreate(0),
            ],
            vec![Op::ChannelDelete(9881)],
            vec![
                Op::ChannelOrder(5_628_462_833_953_539_346),
                Op::Reparent(43_802, 44_013),
            ],
            vec![Op::ChannelEdit(19_457, 4_460_058_725_806_856_635)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::ChannelCreate(0)],
            vec![
                Op::MemberUpsert(1880, 15_790_630_056_925_513_645),
                Op::Cutoff(144),
            ],
            vec![
                Op::Cutoff(140),
                Op::MemberUpsert(37_579, 3_600_077_410_193_205_360),
                Op::VirtualAccess(30_106, vec![]),
            ],
            vec![Op::Roles(10_184_367_242_988_533_185)],
            vec![
                Op::EveryoneEdit(10_979_092_672_964_746_496),
                Op::MemberUpsert(42_115, 15_316_192_735_732_539_403),
                Op::ChannelEdit(6794, 4_465_384_658_157_372_229),
            ],
            vec![Op::ChannelEdit(52_592, 9_071_395_738_716_798_405)],
            vec![Op::MemberRemove(60_840)],
            vec![
                Op::ChannelEdit(18_540, 4_334_990_577_488_062_307),
                Op::MemberRemove(53_929),
            ],
            vec![Op::RoleEdit(117, 9_027_757_780_624_872_436)],
            vec![Op::ChannelEdit(9022, 3_322_984_281_809_535_830)],
            vec![Op::MemberUpsert(63_707, 15_835_593_170_689_658_458)],
            vec![
                Op::Roles(7_615_537_905_488_844_484),
                Op::EveryoneEdit(3_183_442_360_755_587_764),
                Op::MemberUpsert(10_504, 17_228_001_812_846_514_319),
            ],
            vec![
                Op::Roles(17_747_081_521_088_645_116),
                Op::RoleEdit(153, 16_731_338_437_983_829_544),
                Op::ChannelEdit(34_140, 16_899_414_156_632_336_507),
            ],
            vec![Op::MemberUpsert(25_270, 8_624_319_659_619_373_279)],
            vec![
                Op::ChannelEdit(58_058, 3_595_593_565_693_777_495),
                Op::ChannelEdit(6608, 5_040_848_093_841_694_769),
            ],
            vec![Op::MemberRemove(35_996), Op::Owner(Some(15_881))],
            vec![
                Op::ChannelEdit(27_248, 9_725_196_681_347_329_928),
                Op::ChannelEdit(41_373, 12_208_065_489_403_227_201),
            ],
            vec![Op::MemberRemove(8772), Op::RolePosition(88, 3)],
            vec![Op::MemberUpsert(13_803, 1_774_340_070_951_364_836)],
            vec![
                Op::ChannelEdit(18_560, 10_186_621_050_444_680_395),
                Op::RoleEdit(81, 15_768_247_240_366_192_422),
                Op::RolePosition(42, 3),
            ],
            vec![
                Op::ChannelEdit(29_450, 10_052_188_259_761_505_317),
                Op::VirtualAccess(15_930, vec![]),
                Op::MemberUpsert(32_464, 16_363_651_873_588_988_974),
            ],
            vec![
                Op::ChannelEdit(2772, 12_781_955_987_057_759_769),
                Op::MemberRemove(35_028),
            ],
            vec![
                Op::MemberUpsert(42_074, 11_240_420_247_111_129_937),
                Op::MemberUpsert(24_128, 5_034_026_213_597_201_431),
                Op::MemberRemove(53_872),
            ],
        ],
    );
}

#[test]
fn shrunk_wip_seed_3() {
    replay(
        Params {
            roles: 5,
            channels: 18,
            members: 1,
            seed: 481_021_231_772_572_136,
            budget: 250_000,
            floor: 4096,
            cache: 1_048_576,
        },
        vec![
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelDelete(0)],
            vec![Op::ChannelCreate(23_806_692_179_414_899)],
            vec![Op::ChannelOrder(3_570_962_000_863_171_220)],
            vec![Op::Reparent(33_888, 63_286)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelCreate(0), Op::ChannelCreate(0)],
            vec![Op::ChannelEdit(30_855, 12_258_629_550_844_811_365)],
            vec![Op::Roles(0)],
            vec![Op::MemberUpsert(0, 7_838_845_317_191_833_938)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::ChannelOrder(538_458_669_621_193)],
            vec![Op::MemberUpsert(29_233, 8_028_812_829_376_677_307)],
            vec![
                Op::MemberUpsert(40_304, 17_083_251_891_138_393_440),
                Op::ChannelEdit(62_733, 6_936_266_656_800_676_162),
            ],
            vec![
                Op::ChannelEdit(12_285, 6_939_345_765_243_786_196),
                Op::ChannelEdit(5826, 2_411_974_930_627_484_386),
                Op::Roles(11_987_260_856_239_485_375),
            ],
            vec![
                Op::MemberUpsert(6133, 13_365_489_726_618_101_006),
                Op::VirtualAccess(42_122, vec![23_737, 32_480]),
                Op::RolePosition(3, 2),
            ],
            vec![Op::VirtualAccess(22_638, vec![43_627, 15_785])],
            vec![
                Op::ChannelEdit(43_215, 17_799_899_442_357_063_297),
                Op::Reparent(4612, 51_701),
            ],
            vec![
                Op::EveryoneEdit(4_536_178_897_288_067_614),
                Op::ChannelEdit(58_623, 9_692_996_116_828_235_887),
            ],
            vec![
                Op::MemberUpsert(7744, 17_249_350_728_010_710_009),
                Op::MemberUpsert(30_325, 16_201_223_098_711_061_533),
                Op::Roles(3_221_377_684_070_845_931),
            ],
            vec![
                Op::MemberUpsert(20_881, 10_469_690_662_629_587_049),
                Op::MemberUpsert(57_688, 8_344_882_720_277_083_149),
            ],
            vec![Op::ChannelEdit(43_990, 10_315_972_272_490_221_816)],
            vec![
                Op::Reparent(30_301, 16_394),
                Op::VirtualAccess(40_829, vec![14_773]),
            ],
            vec![
                Op::RoleEdit(209, 12_284_097_201_304_915_102),
                Op::MemberUpsert(53_855, 7_475_353_075_185_032_337),
                Op::ChannelDelete(27_261),
            ],
            vec![
                Op::Roles(3_896_172_149_896_337_072),
                Op::MemberUpsert(8363, 6_198_332_516_035_346_512),
                Op::ChannelDelete(6429),
            ],
            vec![Op::ChannelCreate(13_724_267_118_559_183_426)],
            vec![
                Op::MemberUpsert(23_483, 13_956_106_128_999_040_845),
                Op::ChannelDelete(49_177),
                Op::RoleEdit(33, 13_142_456_172_549_132_017),
            ],
            vec![
                Op::EveryoneEdit(10_775_702_480_786_012_402),
                Op::EveryoneEdit(10_946_241_619_115_526_723),
                Op::RoleEdit(184, 17_531_501_239_191_315_727),
            ],
            vec![
                Op::ChannelEdit(8749, 2_889_098_774_709_034_586),
                Op::MemberUpsert(23_593, 8_136_384_191_123_554_086),
            ],
            vec![
                Op::Roles(12_211_917_568_518_015_860),
                Op::ChannelCreate(7_549_800_352_619_536_592),
            ],
            vec![
                Op::MemberUpsert(8773, 3_634_247_013_764_118_981),
                Op::MemberUpsert(23_445, 5_829_822_575_682_641_876),
            ],
            vec![
                Op::RoleEdit(47, 8_009_040_025_538_257_001),
                Op::Roles(11_624_592_522_024_575_592),
            ],
            vec![
                Op::MemberUpsert(40_900, 12_969_373_372_224_892_519),
                Op::MemberUpsert(50_685, 11_909_906_919_486_166_932),
            ],
        ],
    );
}

#[test]
fn shrunk_wip_seed_4() {
    replay(
        Params {
            roles: 1,
            channels: 1,
            members: 1,
            seed: 2_234_782_473_979_763,
            budget: 250_000,
            floor: 4096,
            cache: 1_048_576,
        },
        vec![
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelDelete(0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelDelete(0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::Roles(0)],
            vec![Op::ChannelEdit(0, 0)],
            vec![Op::ChannelOrder(0)],
            vec![Op::Roles(0)],
            vec![Op::ChannelCreate(0), Op::ChannelCreate(0)],
            vec![Op::ChannelOrder(0)],
            vec![Op::ChannelOrder(0)],
            vec![Op::ChannelCreate(16_670_540_134_646_853_712)],
            vec![Op::ChannelEdit(8090, 0)],
            vec![Op::Roles(0)],
            vec![Op::MemberUpsert(0, 0)],
            vec![Op::Roles(0)],
            vec![Op::MemberUpsert(0, 0)],
            vec![Op::MemberUpsert(0, 0)],
            vec![Op::Reparent(7334, 30_298)],
            vec![Op::ChannelEdit(185, 0)],
            vec![Op::ChannelOrder(4_713_461_670_454_784_202)],
            vec![Op::Reparent(150, 0)],
            vec![Op::MemberUpsert(0, 0)],
            vec![Op::MemberUpsert(0, 0)],
            vec![Op::ChannelEdit(1374, 3_271_267_551_228_833_960)],
            vec![Op::Roles(18_127_401_726_936_066)],
            vec![Op::MemberUpsert(0, 0)],
            vec![Op::ChannelOrder(3_947_676_766_879_639_803)],
            vec![Op::MemberUpsert(0, 0)],
            vec![Op::Roles(5_687_475_109_452_407_478)],
            vec![Op::ChannelOrder(346_033_117_210_378_295)],
            vec![Op::ChannelOrder(666_992_466_182_499_694)],
            vec![
                Op::RoleEdit(85, 264_818_700_318_013_331),
                Op::ChannelEdit(314, 6_938_221_541_221_116_228),
            ],
            vec![Op::ChannelCreate(29_573)],
            vec![
                Op::ChannelEdit(228, 8_102_546_808_684_363_360),
                Op::Roles(4_196_318_538_822_384_399),
            ],
            vec![Op::EveryoneEdit(2_696_271_690_098_737_257)],
            vec![
                Op::MemberUpsert(6444, 1_629_817_338_181_594_137),
                Op::MemberUpsert(2190, 195_676_397_330_986_479),
            ],
            vec![Op::MemberRemove(47_639)],
            vec![
                Op::RoleEdit(2, 12_385_515_456_298_216_961),
                Op::RoleEdit(222, 2_197_981_105_272_731_258),
            ],
            vec![
                Op::Owner(None),
                Op::RoleEdit(118, 18_069_346_720_970_204_152),
                Op::MemberRemove(46_482),
            ],
        ],
    );
}

#[test]
fn shrunk_wip_seed_5() {
    replay(
        Params {
            roles: 12,
            channels: 3,
            members: 1,
            seed: 7_975_530_815_916_739_654,
            budget: 250_000,
            floor: 4096,
            cache: 1_048_576,
        },
        vec![
            vec![Op::ChannelOrder(5_056_564_249_862_002_044)],
            vec![Op::Nsfw(230)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::ChannelCreate(11_028_974_531_673_523_460)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelEdit(39_178, 8_301_719_556_487_362_295)],
            vec![Op::ChannelEdit(13_466, 15_597_100_151_653_437_263)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::EveryoneEdit(577_791_439_172_558_904)],
            vec![
                Op::Owner(Some(8417)),
                Op::RoleEdit(98, 10_614_122_426_420_421_206),
            ],
            vec![Op::Roles(16_216_341_004_451_968_393)],
            vec![Op::MemberRemove(24_460)],
            vec![
                Op::ChannelCreate(14_175_910_109_998_679_465),
                Op::ChannelEdit(30_161, 14_220_121_751_129_251_628),
            ],
            vec![
                Op::MemberUpsert(58_032, 6_320_573_557_738_149_467),
                Op::MemberUpsert(41_724, 10_209_012_889_070_254_842),
            ],
            vec![Op::ChannelOrder(9_679_220_384_614_041_362)],
            vec![
                Op::MemberUpsert(54_810, 13_486_277_533_090_630_802),
                Op::RolePosition(86, 15),
                Op::ChannelEdit(32_929, 7_196_678_362_700_192_636),
            ],
            vec![Op::ChannelOrder(1_141_428_533_155_327_620)],
            vec![
                Op::ChannelEdit(51_291, 4_656_927_166_849_647_174),
                Op::MemberUpsert(60_021, 3_880_718_002_891_764_572),
                Op::ChannelEdit(51_106, 346_284_402_999_100_308),
            ],
            vec![Op::Nsfw(235)],
            vec![Op::MemberUpsert(28_236, 14_663_841_645_698_303_386)],
            vec![
                Op::MemberUpsert(35_228, 12_191_066_482_161_882_296),
                Op::EveryoneEdit(12_165_998_302_225_658_870),
            ],
            vec![Op::ChannelDelete(12_298)],
            vec![
                Op::EveryoneEdit(15_925_769_586_509_367_220),
                Op::ChannelCreate(7_344_732_790_536_801_139),
            ],
            vec![Op::Roles(8_026_733_966_582_675_491)],
            vec![Op::VirtualAccess(11_131, vec![]), Op::Nsfw(247)],
            vec![
                Op::Reparent(9156, 2490),
                Op::MemberUpsert(50_404, 7_832_531_074_455_735_431),
            ],
            vec![
                Op::RoleEdit(177, 14_564_957_913_073_968_220),
                Op::ChannelEdit(61_220, 17_257_434_968_727_258_219),
            ],
            vec![
                Op::RoleEdit(98, 17_644_756_041_771_310_543),
                Op::MemberRemove(25_284),
            ],
            vec![Op::MemberRemove(26_134)],
            vec![
                Op::MemberUpsert(47_435, 16_640_668_353_370_510_270),
                Op::MemberUpsert(31_320, 4_179_838_411_614_738_676),
            ],
            vec![
                Op::MemberRemove(41_580),
                Op::ChannelEdit(30_307, 8_952_017_961_131_090_616),
            ],
        ],
    );
}

#[test]
fn shrunk_wip_seed_6() {
    replay(
        Params {
            roles: 2,
            channels: 3,
            members: 1,
            seed: 10_113_271_290_653_952_161,
            budget: 250_000,
            floor: 4096,
            cache: 1_048_576,
        },
        vec![
            vec![Op::ChannelCreate(13_575_084_895_325_593_309)],
            vec![Op::Roles(0)],
            vec![Op::ChannelEdit(1, 0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelEdit(36_830, 9_797_296_555_528_396_459)],
            vec![Op::ChannelCreate(0)],
            vec![Op::Reparent(44_462, 14_360), Op::ChannelDelete(23_478)],
            vec![Op::ChannelDelete(21_566)],
            vec![Op::Roles(16_541_983_649_850_501)],
            vec![Op::ChannelEdit(22_407, 6_004_867_787_982_451_466)],
        ],
    );
}

#[test]
fn shrunk_wip_seed_7() {
    replay(
        Params {
            roles: 6,
            channels: 2,
            members: 6,
            seed: 1_077_722_704_226,
            budget: 250_000,
            floor: 4096,
            cache: 1_048_576,
        },
        vec![
            vec![Op::ChannelDelete(0)],
            vec![Op::ChannelCreate(0), Op::ChannelCreate(0)],
            vec![Op::ChannelEdit(196, 0)],
            vec![Op::Roles(0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelCreate(1_383_023_938_071_735_144)],
            vec![Op::ChannelDelete(0)],
            vec![Op::ChannelDelete(0)],
            vec![Op::ChannelDelete(0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelDelete(0)],
            vec![Op::Roles(0)],
            vec![Op::Reparent(20_220, 14_759)],
            vec![Op::ChannelEdit(18_390, 14_478_530_596_698_516_734)],
            vec![Op::Roles(0)],
            vec![Op::ChannelOrder(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(13_483_121_950_151_129_413)],
            vec![Op::ChannelOrder(0)],
            vec![Op::RoleEdit(74, 1_594_098_424_106_937_673)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::RoleEdit(0, 8_944_331_561_150)],
            vec![
                Op::RoleEdit(112, 296_367_547_485_464_112),
                Op::MemberRemove(22_354),
            ],
            vec![
                Op::ChannelDelete(51_139),
                Op::MemberUpsert(19_143, 11_984_137_662_885_883_138),
                Op::MemberUpsert(60_398, 15_603_436_271_386_802_736),
            ],
            vec![
                Op::RoleEdit(63, 7_191_376_555_037_814_600),
                Op::RoleEdit(81, 5_787_556_510_919_447_713),
            ],
            vec![Op::MemberUpsert(1170, 8_109_598_113_044_505_308)],
            vec![Op::ChannelEdit(53_159, 146_603_005_457_891_150)],
            vec![
                Op::MemberUpsert(8989, 8_372_800_161_176_474_505),
                Op::MemberUpsert(12_031, 3_385_774_191_106_251_968),
            ],
            vec![
                Op::EveryoneEdit(10_921_761_319_023_104_161),
                Op::EveryoneEdit(8_261_468_468_382_656_502),
            ],
            vec![
                Op::VirtualAccess(30_291, vec![]),
                Op::ChannelEdit(34_334, 761_650_632_007_837_690),
                Op::Reparent(46_463, 49_404),
            ],
            vec![
                Op::Reparent(55_452, 37_018),
                Op::ChannelEdit(60_147, 13_231_804_056_898_597_663),
                Op::RolePosition(82, 7),
            ],
            vec![
                Op::MemberUpsert(5661, 1_230_857_542_183_823_606),
                Op::VirtualAccess(32_055, vec![60_794, 5335]),
            ],
            vec![Op::MemberUpsert(14_150, 16_546_633_238_693_205_953)],
            vec![
                Op::ChannelDelete(52_045),
                Op::Cutoff(106),
                Op::MemberUpsert(26_027, 5_761_561_176_849_626_073),
            ],
            vec![
                Op::ChannelCreate(4_929_441_991_711_831_687),
                Op::MemberUpsert(18_602, 5_526_989_101_832_427_775),
            ],
            vec![
                Op::MemberRemove(508),
                Op::Roles(15_811_108_559_900_572_940),
                Op::Nsfw(126),
            ],
            vec![
                Op::RoleEdit(119, 8_865_337_296_419_921_572),
                Op::MemberUpsert(25_191, 2_192_111_688_876_372_824),
                Op::MemberUpsert(954, 7_893_796_829_216_447_927),
            ],
            vec![Op::ChannelEdit(23_404, 6_029_122_511_820_115_760)],
        ],
    );
}

#[test]
fn shrunk_wip_seed_8() {
    replay(
        Params {
            roles: 6,
            channels: 10,
            members: 1,
            seed: 8_421_913_398_342_960_222,
            budget: 250_000,
            floor: 4096,
            cache: 1_048_576,
        },
        vec![
            vec![Op::ChannelOrder(13_524_144_373_080_391_541)],
            vec![Op::ChannelDelete(2882)],
            vec![Op::ChannelDelete(2864)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::Reparent(62_555, 55_010)],
            vec![Op::ChannelDelete(8126)],
            vec![Op::ChannelEdit(59_882, 13_197_006_073_754_539_592)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
        ],
    );
}

#[test]
fn shrunk_wip_seed_9() {
    replay(
        Params {
            roles: 1,
            channels: 4,
            members: 1,
            seed: 262_291_154_234_181,
            budget: 250_000,
            floor: 4096,
            cache: 1_048_576,
        },
        vec![
            vec![Op::MemberUpsert(0, 0)],
            vec![Op::ChannelDelete(7701)],
            vec![
                Op::ChannelCreate(3_377_633_338_709_610_149),
                Op::ChannelEdit(24_705, 1_883_273_400_021_585_557),
            ],
            vec![Op::ChannelCreate(934_028_350_665_236_576)],
            vec![Op::ChannelDelete(0)],
            vec![Op::ChannelCreate(8_185_798_078_107_645_221)],
            vec![Op::ChannelCreate(5_470_448_641_918_008_632)],
            vec![Op::MemberUpsert(0, 0)],
            vec![Op::ChannelCreate(0)],
            vec![Op::ChannelDelete(5572)],
            vec![Op::ChannelEdit(2758, 2_246_801_526_300_073_842)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::Roles(0)],
            vec![Op::ChannelCreate(1)],
            vec![
                Op::MemberRemove(22_849),
                Op::ChannelCreate(4_539_428_281_931_447_684),
                Op::RoleEdit(23, 15_021_993_788_753_389_085),
            ],
            vec![Op::MemberRemove(21_839)],
            vec![Op::MemberUpsert(29_569, 13_620_846_687_248_176_696)],
            vec![Op::RoleEdit(21, 16_929_644_520_623_865_035)],
            vec![Op::MemberUpsert(33_115, 7_679_057_470_888_776_565)],
            vec![
                Op::MemberRemove(51_834),
                Op::RoleEdit(251, 17_201_575_826_547_442_116),
            ],
            vec![
                Op::MemberUpsert(41_853, 9_170_891_586_625_203_543),
                Op::EveryoneEdit(143_235_313_245_381_598),
            ],
            vec![
                Op::ChannelEdit(37_024, 13_541_994_394_775_742_780),
                Op::VirtualAccess(37_852, vec![]),
            ],
            vec![
                Op::ChannelEdit(51_898, 17_573_957_532_236_069_241),
                Op::RoleEdit(237, 12_291_356_075_478_859_998),
                Op::ChannelDelete(65_483),
            ],
            vec![Op::ChannelDelete(42_965)],
            vec![
                Op::ChannelEdit(40_817, 6_808_338_328_185_707_204),
                Op::Reparent(18_676, 64_539),
                Op::ChannelEdit(3315, 6_077_018_015_901_190_412),
            ],
            vec![Op::Reparent(53_243, 6534)],
            vec![Op::MemberUpsert(22_342, 7_309_479_391_535_847_842)],
            vec![Op::MemberUpsert(29_373, 13_528_971_960_668_523_328)],
            vec![Op::ChannelEdit(43_915, 9_536_012_386_193_349_862)],
            vec![
                Op::VirtualAccess(36_690, vec![33_759]),
                Op::RolePosition(58, 11),
                Op::MemberUpsert(30_011, 17_728_087_792_997_212_471),
            ],
            vec![Op::Roles(15_399_684_340_380_856_712)],
            vec![
                Op::ChannelDelete(55_738),
                Op::MemberUpsert(31_638, 15_594_435_186_169_196_979),
                Op::ChannelDelete(33_134),
            ],
            vec![Op::MemberUpsert(7299, 3_296_509_808_751_307_468)],
            vec![
                Op::ChannelDelete(28_104),
                Op::ChannelOrder(12_395_188_646_117_741_134),
            ],
            vec![
                Op::ChannelEdit(33_381, 14_970_976_966_951_896_714),
                Op::ChannelDelete(26_808),
                Op::MemberUpsert(3619, 15_608_410_123_256_054_264),
            ],
        ],
    );
}

#[test]
fn load_estimate_counts_repeated_role_specs() {
    let mut snapshot = Snapshot::new(guild(), None);
    snapshot.roles = (0..18)
        .map(|_| RoleSpec {
            id: RoleId::new(0),
            permissions: Permissions::empty(),
            position: RankPos::Undefined,
            hoist: Hoist::NotHoisted,
        })
        .collect();
    let params = Params {
        roles: 30,
        channels: 24,
        members: 2013,
        seed: 91_259_465_105_408,
        budget: 250_000,
        floor: 4096,
        cache: 1 << 20,
    };
    let driver = Driver::start(&params, Some(&snapshot));
    check_view(&driver);
}
