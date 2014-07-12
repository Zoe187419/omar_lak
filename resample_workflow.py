def distinguish_sample(population):
    """
    Distinguishes the sample that received the treatment from the potential
    matching population, discarding all others.
    :param population: A list of subjects
    :return: (sample,potential_matches)
    """
    assert (isinstance(population, list))
    pass


def pairwise_match(sample, population):
    """
    Pairwise matches individuals in sample to persons in population, with or
    without replacement.
    :param sample: A list of subjects
    :param population: A list of non-subjects
    :return: A list of matches where sample[i] is matched to return[i]
    """
    assert (isinstance(population, list))
    assert (isinstance(sample, list))
    assert (len(sample) > 0 & & len(population) > 0)
    pass

def match_statistics(sample, matches):
    """
    Compares the sample and matches and generates a set of statistics suitable
    for determining the level at which bias was removed.
    :param sample: A list of subjects
    :param matches: A list of matched non-subjects such that sample[i] is matched
    to maches[i]
    :return: Statistics suitable for determining selection bias removal
    """

def execute():
    """
    Runs this workflow
    :return: None
    """
    sample, population = distinguish_sample(population)
    matches = pairwise_match(sample, population)
    statistics = match_statistics(sample, matches)